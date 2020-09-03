
# 调用R包
library("FactoMineR");
library("factoextra");

data(decathlon2)
head(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

# 获得数据
library("FactoMineR")
library("factoextra")
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]

# PCA计算
res.pca <- PCA(decathlon2.active, graph = FALSE)
# 提取变量的分析结果
var <- get_pca_var(res.pca)
var

# Coordinates of variables
head(var$coord, 4)
# col.var设定线条颜色
fviz_pca_var(res.pca, col.var = "black")

head(var$cos2)
library("corrplot")
# is.corr表示输入的矩阵不是相关系数矩阵
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
# 在第一第二主成分是显示结果（通过值的叠加显示）
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )

# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

head(var$contrib, 4)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
# 各变量对第一主成分的贡献
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
# 各变量对第二主成分的贡献
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )

# Change the transparency by contrib values
fviz_pca_var(res.pca, alpha.var = "contrib")

# Create a random continuous variable of length 10
# 生成随机数
set.seed(123)
my.cont.var <- rnorm(10)
# Color variables by the continuous variable
# col.var设置颜色
# gradient.cols设置颜色渐变范围
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
# 进行聚类
# center聚类数量
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
# 将向量编码为因子
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#proba用于表征维度的显着性阈值，
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1 第一主成分
res.desc$Dim.1

# 第二主成分
res.desc$Dim.2

ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca)

# Quality of individuals
head(ind$cos2)
# repel=TRUE能够避免部分重合的点重叠
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )

fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )

fviz_cos2(res.pca, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

head(iris, 3)

# The variable Species (index = 5) is removed before PCA analysis
# 第5列不进行PCA运算
iris.pca <- PCA(iris[,-5], graph = FALSE)

fviz_pca_ind(iris.pca,
             # show points only (nbut not "text") 只显示点而不显示文本，默认都显示
             geom.ind = "point", 
             # 设定分类种类
             col.ind = iris$Species,
             # 设定颜色
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             # 添加椭圆 Concentration ellipses
             addEllipses = TRUE,
             legend.title = "Groups",
             )

fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco"
             )

# Variables on dimensions 2 and 3
fviz_pca_var(res.pca, axes = c(2, 3))
# Individuals on dimensions 2 and 3
fviz_pca_ind(res.pca, axes = c(2, 3))

# Show variable points and text labels
fviz_pca_var(res.pca, geom.var = c("point", "text"))

# Show individuals text labels only
fviz_pca_ind(res.pca, geom.ind =  "text")

# Change the size of arrows an labels
fviz_pca_var(res.pca, arrowsize = 1, labelsize = 5, 
             repel = TRUE)
# Change points size, shape and fill color
# Change labelsize
fviz_pca_ind(res.pca, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

# Add confidence ellipses
fviz_pca_ind(iris.pca, geom.ind = "point", 
             # 使用iris数据集
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
             )
# Convex hull
fviz_pca_ind(iris.pca, geom.ind = "point",
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             # 用凸包多边形代替椭圆
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
             )


fviz_pca_var(res.pca, axes.linetype = "dotted")

ind.p <- fviz_pca_ind(iris.pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              # 下标题
              subtitle = "Iris data set",
              # 说明
              caption = "Source: factoextra",
              # x,y轴标题
              xlab = "PC1", ylab = "PC2",
              # 标题名字位置
              legend.title = "Species", legend.position = "top",
              # 主题和配设
              ggtheme = theme_gray(), palette = "jco"
              )

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )

fviz_pca_biplot(iris.pca, 
                # 观测量颜色
                col.ind = iris$Species, palette = "jco", 
                # 添加椭圆
                addEllipses = TRUE, label = "var",
                # 线条颜色
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

fviz_pca_biplot(iris.pca, 
                # Fill individuals by groups
                geom.ind = "point",
                # 点的形状
                pointshape = 21,
                # 点的大小
                pointsize = 2.5,
                # 按照组类特定形状
                fill.ind = iris$Species,
                col.ind = "black",
                # Color variable by groups
                # 颜色
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                # 标题
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
             )+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

fviz_pca_biplot(iris.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
                )

# Visualize variable with cos2 >= 0.6
# 可视化cos2>0.6
fviz_pca_var(res.pca, select.var = list(cos2 = 0.6))

# Select by names
# 根据名字显示
name <- list(name = c("Long.jump", "High.jump", "X100m"))
fviz_pca_var(res.pca, select.var = name)

# 根据前五贡献
# top 5 contributing individuals and variable
fviz_pca_biplot(res.pca, select.ind = list(contrib = 5), 
               select.var = list(contrib = 5),
               ggtheme = theme_minimal())
