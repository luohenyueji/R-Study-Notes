
# 加载库
library(WGCNA);

# 读取文件
# The following setting is important, do not omit.
# 如果没有显式地指定“stringsAsFactors=FALSE”,默认会将所有的字符串转换为因子,导致数据处理速度较慢
options(stringsAsFactors = FALSE)
# Read in the female liver data set 读取135个雌性小鼠的数据
femData = read.csv("./data/LiverFemale3600.csv")
# Take a quick look at what is in the data set:
# 查看数据的维度
dim(femData)
# 预览数据
head(femData)

#  删除冗余数据-c(1:8)删除前8列数据，t()转置数据
datExpr0 = as.data.frame(t(femData[, -c(1:8)]));
head(datExpr0)

# 将原数据的行列名复制过来
names(datExpr0) = femData$substanceBXH;
rownames(datExpr0) = names(femData)[-c(1:8)];
head(datExpr0)

gsg = goodSamplesGenes(datExpr0, verbose = 3)
gsg$allOK;

if (!gsg$allOK)
{
    # Optionally, print the gene and sample names that were removed:
    # 打印删除的基因和样本名称
    if (sum(!gsg$goodGenes)>0)
        printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")));
    if (sum(!gsg$goodSamples)>0)
        printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
    # Remove the offending genes and samples from the data:
    # 从数据中删除有问题的基因和样本
    datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
}

# hclusts聚类算法, dist计算基因之间的距离
sampleTree = hclust(dist(datExpr0), method = "average");
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
# 绘制聚类树，sizeGrWindow设置绘图窗口大小
# sizeGrWindow(16,9)
pdf(file = "./plot/sampleClustering.pdf", width = 12, height = 9);
# 设置文字大小
par(cex = 0.5);
# 设置图像边距c(bottom, left, top, right) 
# par(mar = c(0,4,2,0))
# 画图 main标题，sub子标题，xlab x轴标题，cex.lab标题字体大小，cex.axis坐标轴刻度大小，cex.main主标题字体
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
dev.off()

# Plot a line to show the cut
# 设置文字大小
par(cex = 0.5);
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
# 在上图上画红线
abline(h = 15, col = "red");
# Determine cluster under the line
# 剪枝算法，cutHeight 修剪树枝的高度 minSize集群最小数
clust = cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
# 剪枝结果
table(clust)
# clust 1 contains the samples we want to keep
keepSamples = (clust==1)
# 符合要求的数据
datExpr = datExpr0[keepSamples, ]
# 提取行
nSamples = nrow(datExpr)
# 提取列
nGenes = ncol(datExpr)

traitData = read.csv("./data/ClinicalTraits.csv");
dim(traitData)
#names(traitData)
# remove columns that hold information we do not need.
# 删除不需要的列
allTraits = traitData[, -c(31, 16)];
allTraits = allTraits[, c(2, 11:36) ];
dim(allTraits)
head(allTraits)
# names(allTraits)

# 形成一个类似于表达数据的数据框架，以保存临床特征
# 提取行名
femaleSamples = rownames(datExpr)
# 数据匹配 返回匹配行
traitRows = match(femaleSamples, allTraits$Mice);
# 提取指定要求行
datTraits = allTraits[traitRows, -1];
# 提取行名
rownames(datTraits) = allTraits[traitRows, 1];
# 垃圾回收
collectGarbage();

# Re-cluster samples
# 画聚类图
sampleTree2 = hclust(dist(datExpr), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
# 画表型的热图
# 将特征转换为颜色表示：白色表示低，红色表示高，灰色表示缺少条目
# 如果signed为true 以绿色开头代表最大负值，以白色开头代表零附近的值，然后变为红色代表正值
traitColors = numbers2colors(datTraits, signed =FALSE);
# Plot the sample dendrogram and the colors underneath.
# 绘制出树状图和下面的颜色 
plotDendroAndColors(sampleTree2, traitColors,groupLabels = names(datTraits),main = "Sample dendrogram and trait heatmap")

# Choose a set of soft-thresholding powers
# 给出候选的β值，c(1:10)表示1到10；seq(from = 12, to=20, by=2)表示从12开始间隔两个数到20
powers = c(c(1:10), seq(from = 12, to=20, by=2))
powers
# Call the network topology analysis function 调用网络拓扑分析函数
# verbose表示输出结果详细程度
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 0);

# sft这中保存了每个powers值计算出来的网络特征,其中powerEstimate就是最佳power值，fitIndices保存了每个power对应的网络的特征。
str(sft)

# Plot the results 结果绘图
# 设置窗格大小
#sizeGrWindow(9, 5)
# 设置图的显示一行两列
# par(mfrow = c(1,2));
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
# 生成阈值和网络的特征之间的关系函数
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")

# sft$fitIndices 保存了每个power构建的相关性网络中的连接度的统计值，k就是连接度值，每个power值提供了max, median, max3种连接度的统计量
# 对连接度的均值进行可视化
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

# datExpr表达数据，TOMType拓扑重叠矩阵计算方式，minModuleSize用于模块检测的最小模块尺寸,
# reassignThreshold 是否在模块之间重新分配基因的p值比率阈值，mergeCutHeight 树状图切割高度
# numericLabels 返回的模块应该用颜色（FALSE）还是数字（TRUE）标记,pamRespectsDendro树状图相关参数
# saveTOMs 字符串的向量，saveTOMFileBase 包含包含共识拓扑重叠文件的文件名库的字符串
net = blockwiseModules(datExpr, power = sft$powerEstimate,TOMType = "unsigned", minModuleSize = 30,reassignThreshold = 0, 
                       mergeCutHeight = 0.25,numericLabels = TRUE, pamRespectsDendro = FALSE,saveTOMs = TRUE,
                       saveTOMFileBase = "femaleMouseTOM",verbose = 3)

 table(net$colors)

# open a graphics window
# sizeGrWindow(12, 9)
# Convert labels to colors for plotting
# 将标签转化为绘图颜色
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
# 绘制树状图和下面的模块颜色
# dendroLabels树状图标签。设置为FALSE完全禁用树状图标签；设置为NULL使用的行标签datExpr
# addGuide是否应在树状图中添加垂直的“指导线”？线条使识别单个样本的颜色代码更加容易。
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],"Module colors",
                    dendroLabels = FALSE, hang = 0.03,addGuide = TRUE, guideHang = 0.05)

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(MEs, moduleLabels, moduleColors, geneTree,
file = "FemaleLiver-02-networkConstruction-auto.RData")

# Define numbers of genes and samples
# 获得基因数和样本数
nGenes = ncol(datExpr);
nSamples = nrow(datExpr);

# Recalculate MEs with color labels
# 用彩色标签重新计算MEs
# 在给定的单个数据集中计算模块的模块本征基因
MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes
# 对给定的（特征）向量进行重新排序，以使相似的向量（通过相关性度量）彼此相邻
MEs = orderMEs(MEs0)

# 计算module的ME值与表型的相关系数
moduleTraitCor = cor(MEs, datTraits, use = "p");
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples);

names(MEs)

# sizeGrWindow(10,6)
# 显示相关性及其p值
textMatrix = paste(signif(moduleTraitCor, 2), "\n(",signif(moduleTraitPvalue, 1), ")", sep = "");
dim(textMatrix) = dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3));
# Display the correlation values within a heatmap plot\
# ySymbols 当ylabels使用时所使用的其他标签； colorLabels 应该使用颜色标签吗
# colors 颜色； textMatrix 单元格名字
labeledHeatmap(Matrix = moduleTraitCor,xLabels = names(datTraits),yLabels = names(MEs),ySymbols = names(MEs),
               colorLabels = FALSE,colors = greenWhiteRed(50),textMatrix = textMatrix,setStdMargins = FALSE,
               cex.text = 0.4,zlim = c(-1,1),
main = paste("Module-trait relationships"))

sizeGrWindow(10,6)
# Will display correlations and their p-values

dim(textMatrix) = dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3));
# Display the correlation values within a heatmap plot
labeledHeatmap(Matrix = moduleTraitCor,
xLabels = names(datTraits),
yLabels = names(MEs),
ySymbols = names(MEs),
colorLabels = FALSE,
colors = greenWhiteRed(50),
textMatrix = textMatrix,
setStdMargins = FALSE,
cex.text = 0.5,
zlim = c(-1,1),
main = paste("Module-trait relationships"))

# Define variable weight containing the weight column of datTrait
# 定义包含数据特征权重列的变量权重
weight = as.data.frame(datTraits$weight_g);
names(weight) = "weight"
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
# 模块的名称(颜色) substring提取文本从第3个字母开始
modNames = substring(names(MEs), 3)
# 基因和模块的相关系数
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples));
names(geneModuleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");

#gene和性状的关系
geneTraitSignificance = as.data.frame(cor(datExpr, weight, use = "p"));
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));
names(geneTraitSignificance) = paste("GS.", names(weight), sep="");
names(GSPvalue) = paste("p.GS.", names(weight), sep="");

# 模型颜色
module = "brown"
# 匹配列
column = match(module, modNames);
moduleGenes = moduleColors==module;
#sizeGrWindow(7, 7);
par(mfrow = c(1,1));
# 画散点图
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                    abs(geneTraitSignificance[moduleGenes, 1]),
                    xlab = paste("Module Membership in", module, "module"),
                    ylab = "Gene significance for body weight",
                    main = paste("Module membership vs. gene significance\n"),
                    cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)

# 提取表带数据样本名称
# names(datExpr);
# 指定颜色数据名称
# names(datExpr)[moduleColors=="brown"]

# 基因注释数据
annot = read.csv(file = "./data/GeneAnnotation.csv");
dim(annot)
names(annot)
probes = names(datExpr)
probes2annot = match(probes, annot$substanceBXH)
# The following is the number or probes without annotation:
sum(is.na(probes2annot))

# Create the starting data frame
geneInfo0 = data.frame(substanceBXH = probes,
geneSymbol = annot$gene_symbol[probes2annot],
LocusLinkID = annot$LocusLinkID[probes2annot],
moduleColor = moduleColors,
geneTraitSignificance,
GSPvalue)
# Order modules by their significance for weight
modOrder = order(-abs(cor(MEs, weight, use = "p")));
# Add module membership information in the chosen order
for (mod in 1:ncol(geneModuleMembership))
{
    oldNames = names(geneInfo0)
    geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]],
    MMPvalue[, modOrder[mod]]);
    names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
    paste("p.MM.", modNames[modOrder[mod]], sep=""))
}
# Order the genes in the geneInfo variable first by module color, then by geneTraitSignificance
geneOrder = order(geneInfo0$moduleColor, -abs(geneInfo0$GS.weight));
geneInfo = geneInfo0[geneOrder, ]
write.csv(geneInfo, file = "geneInfo.csv")

# Calculate topological overlap anew: this could be done more efficiently by saving the TOM
# calculated during module detection, but let us do it again here.
# 重新计算拓扑重叠：通过保存TOM可以更有效地完成此操作
# 是在模块检测期间计算的，但让我们在这里再次进行。
dissTOM = 1-TOMsimilarityFromExpr(datExpr, power = 6);
# Transform dissTOM with a power to make moderately strong connections more visible in the heatmap
# 变换dissTOM
plotTOM = dissTOM^7;
# Set diagonal to NA for a nicer plot
diag(plotTOM) = NA;
# Call the plot function
# sizeGrWindow(9,9)
# 基因的聚类树聚类时的距离为1-TOM值结合基因间的距离，即1-TOM值，用热图展示
# TOMplot(plotTOM, geneTree, moduleColors, main = "Network heatmap plot, all genes")

nSelect = 400
# For reproducibility, we set the random seed
set.seed(10);
select = sample(nGenes, size = nSelect);
selectTOM = dissTOM[select, select];
# There’s no simple way of restricting a clustering tree to a subset of genes, so we must re-cluster.
# 重新画聚类图
selectTree = hclust(as.dist(selectTOM), method = "average")
selectColors = moduleColors[select];
# Open a graphical window
# sizeGrWindow(9,9)
# Taking the dissimilarity to a power, say 10, makes the plot more informative by effectively changing
# the color palette; setting the diagonal to NA also improves the clarity of the plot
plotDiss = selectTOM^7;
diag(plotDiss) = NA;
TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected genes")

# Recalculate module eigengenes
# 重新计算基因特征值
MEs = moduleEigengenes(datExpr, moduleColors)$eigengenes
# Isolate weight from the clinical traits
weight = as.data.frame(datTraits$weight_g);
names(weight) = "weight"
# Add the weight to existing module eigengenes
MET = orderMEs(cbind(MEs, weight))
# Plot the relationships among the eigengenes and the trait
#sizeGrWindow(5,7.5);
par(cex = 0.9)
# 画树形图
# marDendro给出树状图的边距设置，marHeatmap热图边距设置
plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle= 90)

# Plot the dendrogram
# sizeGrWindow(6,6);
par(cex = 1.0)
plotEigengeneNetworks(MET, "Eigengene dendrogram", marDendro = c(0,4,2,0),
plotHeatmaps = FALSE)
# Plot the heatmap matrix (note: this plot will overwrite the dendrogram plot)
par(cex = 1.0)
plotEigengeneNetworks(MET, "Eigengene adjacency heatmap", marHeatmap = c(3,4,2,2),plotDendrograms = FALSE, xLabelsAngle = 90)
