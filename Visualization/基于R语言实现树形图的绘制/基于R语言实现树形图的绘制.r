
# libraries
# 包
library(ggraph)
library(igraph)
library(tidyverse)
 
# create an edge list data frame giving the hierarchical structure of your individuals
# 创建层级数据
d1 <- data.frame(from="origin", to=paste("group", seq(1,3), sep=""))
d1
d2 <- data.frame(from=rep(d1$to, each=3), to=paste("subgroup", seq(1,9), sep="_"))
d2
# 汇总
edges <- rbind(d1, d2)
edges

# Create a graph object 
mygraph <- graph_from_data_frame( edges )
mygraph

# Basic tree
# 基础树形图
# layout表示布局方式，circular表示是否为环状树形图
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
    # 画边
    geom_edge_diagonal() +
    # 画节点
    geom_node_point() +
    # 设置主题
    theme_void()

# libraries
library(ggraph)
library(igraph)
library(tidyverse)
 
# create a data frame 
data <- data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)
data

# transform it to a edge list!
edges_level1_2 <- data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level1_2
edges_level2_3
edge_list=rbind(edges_level1_2, edges_level2_3)
edge_list

# Now we can plot that
mygraph<- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_diagonal() +
    geom_node_point() +
    theme_void()

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())
 
# data: edge list
# 边数据
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges <- rbind(d1, d2)

# We can add a second data frame with information for each node!
# 为每个节点设置信息
name <- unique(c(as.character(edges$from), as.character(edges$to)))
# 设置每个节点对应的聚类信息和值
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)
vertices[0:10,]
# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)

# 线形布局
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() 
# 环形布局
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal()

# 折线
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_link()
# 弧线
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal()

ggraph(mygraph, layout = 'dendrogram') + 
    # 设置边
    geom_edge_diagonal() +
    # 设置节点名，label表示节点名，filter=leaf表示跳过叶子节点，angle标签方向，hjust和nudge_y标签和节点距离
    geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.01) +
    # 设置y轴范围
    ylim(-.4, NA)

ggraph(mygraph, layout = 'dendrogram') + 
    geom_edge_diagonal() +
    geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.04) +
    # 为每个节点添加点
    geom_node_point(aes(filter=leaf) , alpha=0.6) +
    ylim(-.5, NA)

ggraph(mygraph, layout = 'dendrogram') + 
    geom_edge_diagonal() +
    geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
    geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
    ylim(-.6, NA) +
    theme(legend.position="none")

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 

# 创建数据，类似前面的步骤
# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d2=data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
edges=rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy
# 为每个节点添加值
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))),
  # 正态分布随机取值,共获得31个值。如果是其他数据，去掉value = runif(31)，查看运行后的dim(vertices)就知道该填多少了
  value = runif(31)
) 
# Let's add a column with the group of each name. It will be useful later to color points
# 为每个节点添加分组信息
vertices$group = edges$from[ match( vertices$name, edges$to ) ]
dim(vertices)
head(vertices)

# Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip calculate the ANGLE of the labels
# 让我们添加有关我们将要添加的标签的信息：角度、水平调整和翻转，计算标签的角度
# 添加id值
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
# 添加角度
vertices$angle= -360 * vertices$id / nleaves
vertices

# calculate the alignment of labels: right or left
# 判断标签是偏向左边还是右边
# hjust表示是否水平翻转
vertices$hjust<-ifelse(vertices$angle < -90 & vertices$angle > -270, 1, 0)
 
# flip angle BY to make them readable
# 是否翻转标签
vertices$angle<-ifelse(vertices$angle < -90 & vertices$angle > -270, vertices$angle+180, vertices$angle)
vertices[12:20,]
# Create a graph object
# 创建图
mygraph <- graph_from_data_frame( edges, vertices=vertices )

vertices

# Make the plot
p<-ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
    # 设置边
    geom_edge_diagonal(colour="grey") +
    # 设置边的颜色
    scale_edge_colour_distiller(palette = "RdPu") +
    # 设置点的标签
    geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
    # 设置点的形状
    geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
    # 控制颜色
    scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
    scale_size_continuous( range = c(0.1,10) ) +
    theme_void() +
    theme( 
        # 不显示图例
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm"),
    ) +
    expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
p
# 保存数据 Save at png
ggsave(p, file="output.png", width=10, height=10,dpi=300)

# Dataset 
data <- matrix( sample(seq(1,2000),200), ncol = 10 )
rownames(data) <- paste0("sample_" , seq(1,20))
colnames(data) <- paste0("variable",seq(1,10))
data
dim(data)

# Euclidean distance
# 计算欧式距离
dist <- dist(data[ , c(4:8)] , diag=TRUE)

# Hierarchical Clustering with hclust
# 分层聚类
hc <- hclust(dist)

# Plot the result
plot(hc)

# store the dedrogram in an object
# 保存聚类结果为dhc变量
dhc <- as.dendrogram(hc)

# set the margin
par(mar=c(4,4,2,2))
# 打印会告诉你分支情况
print(dhc[[2]])
# Plot the Second group
# 绘图
plot(dhc[[2]] , main= "zoom on a part of the dendrogram")

# store the dedrogram in an object
dhc <- as.dendrogram(hc)

# set the margin
par(mar=c(4,4,2,2))

print(dhc[[2]][[1]])
# Plot the Second group
plot(dhc[[2]][[1]] , main= "zoom on a part of the dendrogram")

# Build dataset (just copy and paste, this is NOT interesting)
# 生成数据，可以跳过
sample <- paste(rep("sample_",24) , seq(1,24) , sep="")
specie <- c(rep("dicoccoides" , 8) , rep("dicoccum" , 8) , rep("durum" , 8))
treatment <- rep(c(rep("High",4 ) , rep("Low",4)),3)
data <- data.frame(sample,specie,treatment)
for (i in seq(1:5)){
      gene=sample(c(1:40) , 24 )
      data=cbind(data , gene)
      colnames(data)[ncol(data)]=paste("gene_",i,sep="")
     }
data[data$treatment=="High" , c(4:8)]=data[data$treatment=="High" , c(4:8)]+100
data[data$specie=="durum" , c(4:8)]=data[data$specie=="durum" , c(4:8)]-30
rownames(data) <- data[,1]    
head(data)

# Compute Euclidean distance between samples
dist=dist(data[ , c(4:8)] , diag=TRUE)

# Perfor clustering with hclust
# 聚类并保存结果
hc <- hclust(dist)
dhc <- as.dendrogram(hc)
dhc

# Actually, each leaf of the tree has several attributes, like the color, the shape.. Have a look to it: 
# 选择特别的节点
specific_leaf <- dhc[[1]][[1]][[1]]
specific_leaf
attributes(specific_leaf)

i=0
colLab<-function(n)
{
    # 判断是否为节点
    if(is.leaf(n))
    {
        # 获得节点的属性
        a=attributes(n)
        
        # I deduce the line in the original data, and so the treatment and the specie.
        # 获得该点的信息
        ligne=match(attributes(n)$label,data[,1])
        # 根据自己的结果设置
        treatment=data[ligne,3];
            if(treatment=="Low"){col_treatment="blue"};if(treatment=="High"){col_treatment="red"}
        # 根据种类设置颜色，根据自己的结果设置
        specie=data[ligne,2];
            if(specie=="dicoccoides"){col_specie="red"};if(specie=="dicoccum"){col_specie="Darkgreen"};if(specie=="durum"){col_specie="blue"}
        
        # M odification of leaf attribute
        # 修改节点的属性
        attr(n,"nodePar")<-c(a$nodePar,list(cex=1.5,lab.cex=1,pch=20,col=col_treatment,lab.col=col_specie,lab.font=1,lab.cex=1))
        }
    return(n)
}

# 应用函数
dL <- dendrapply(dhc, colLab)
 
# And the plot
plot(dL , main="structure of the population")
# 图例
legend("topright",
     # 文字
     legend = c("High Nitrogen" , "Low Nitrogen" , "Durum" , "Dicoccoides" , "Dicoccum"), 
     # 颜色
     col = c("red", "blue" , "blue" , "red" , "Darkgreen"), 
     pch = c(20,20,4,4,4), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
     text.col = "black", horiz = FALSE, inset = c(0, 0.1))

# Library
library(tidyverse)
 
# Data
head(mtcars)
 
# Clusterisation using 3 variables
# 聚类，使用管道
mtcars %>% 
  select(mpg, cyl, disp) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend
 
# Plot
# 绘图
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)

# library
library(dendextend)

# 绘图dend是设置函数
dend %>% 
    # Custom branches
    # 自定义树枝的颜色
    set("branches_col", "red") %>% 
    # 自定义树枝宽度
    set("branches_lwd", 3) %>%
    # Custom labels
    # 自定义标签颜色
    set("labels_col", "blue") %>% 
    # 自定义标签字体大小
    set("labels_cex", 0.8) %>%
    plot()

dend %>% 
    # 自定义树枝节点形状
    set("nodes_pch", 20)  %>% 
    # 自定义树枝节点大小
    set("nodes_cex", 1.5) %>% 
    # 自定义树枝节点颜色
    set("nodes_col", "red") %>% 
    plot()

dend %>% 
    # 最后一层节点形状
    set("leaves_pch", 22)  %>% 
    # 最后一层节点宽度
    set("leaves_cex", 1) %>%
    # 最后一层节点颜色
    set("leaves_col", "red") %>% 
    plot()


par(mar=c(1,1,1,7))
dend %>%
    # 根据第一层分支结果自定义标签颜色
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    # 根据第一层分支结果自定义分支颜色
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
    # horize是否水平放置,axes是否显示旁边的距离尺
    plot(horiz=TRUE, axes=FALSE)
# 画线条，v高度，lty线条类型
abline(v = 350, lty = 2)

# 使用
par(mar=c(9,1,1,1))
dend %>%
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
    plot(axes=FALSE)
# 画矩形框
# k表示将类切割为k个簇，lty矩形框线条类型，lwd矩形框线条宽度，col填充颜色,x表示从第几个类开始画簇
rect.dendrogram( dend, k=3, lty = 2, lwd = 5, x=17, col=rgb(0.1, 0.2, 0.4, 0.1) ) 


# Create a vector of colors, darkgreen if am is 0, green if 1.
# 获得数据，如果am为0就是forestgreen颜色
my_colors <- ifelse(mtcars$am==0, "forestgreen", "green")
 
# Make the dendrogram
# 设置图像空白区域
par(mar=c(10,1,1,1))
dend %>%
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
    set("leaves_pch", 19)  %>% 
    set("nodes_cex", 0.7) %>% 
    plot(axes=FALSE,horiz =FALSE)
 
# Add the colored bar
# 添加颜色bar
# colors颜色，dend聚类图，rowLabels名字
colored_bars(colors = my_colors, dend = dend, rowLabels = "am",horiz =FALSE)

# Make 2 dendrograms, using 2 different clustering methods
# 使用两种完全不同的聚类方法
d1 <- USArrests %>% dist() %>% hclust( method="average" ) %>% as.dendrogram()
d2 <- USArrests %>% dist() %>% hclust( method="complete" ) %>% as.dendrogram()
 
# Custom these kendo, and place them in a list
# 定制树列表
dl <- dendlist(
  d1 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3),
  d2 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3)
)
 
# Plot them together
tanglegram(dl, 
            # 子树是否带颜色
            common_subtrees_color_lines = FALSE, 
            # 是否突出显示边
            highlight_distinct_edges  = TRUE, 
            # 是否突出分支
            highlight_branches_lwd=FALSE, 
            # 两个树的距离
            margin_inner=7,
            # 两个树之间线条宽度
            lwd=2
)
