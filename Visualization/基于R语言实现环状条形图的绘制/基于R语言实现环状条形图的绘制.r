
# Libraries
# 导入包
library(tidyverse)
 
# Create dataset
# 创建数据
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)
head(data)

# Make the plot
# 画图
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       
    # This add the bars with a blue color
    # 添加蓝色条形，stat表示数据统计方式，也就是说identity提取横坐标x对应的y值
    geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
    # The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    # 设置y的范围，负值设定内圆的大小，正值设定各个条柱的最高高度
    ylim(-100,120)+
    # theme_minimal简约主题
    theme_minimal() +
    # Custom the theme: no axis title and no cartesian grid
    # 自定义主题
    theme(
        # 移除标题坐标文字
        axis.text = element_blank(),
        axis.title = element_blank(),
        # 移除网格
        panel.grid = element_blank(),
        # This remove unnecessary margin around plot
        # 移除不必要空白
        plot.margin = unit(rep(-2,4), "cm"))+
    # This makes the coordinate polar instead of cartesian.
    # 使用极坐标系
    coord_polar(start = 0)
p

# Libraries
library(tidyverse)
 
# Create dataset
# 创建数据
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)
# ----- This section prepare a dataframe for labels ---- #
# 准备数据标签
# Get the name and the y position of each label
label_data <- data
# calculate the ANGLE of the labels
# 计算标签角度
number_of_bar <- nrow(label_data)
number_of_bar

# I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# 减去0.5是为了让标签位于条柱中心
# angle是标签角度
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar 

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
# 判断标签左对齐还是右对齐，也就是标签是朝向左边还是右边
label_data$hjust<-ifelse( angle < -90, 1, 0)
 
# flip angle BY to make them readable
# 翻转标签
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #
head(label_data)


# Start the plot
# 开始绘图
p <- ggplot(data, aes(x=as.factor(id), y=value)) +   
    # This add the bars with a bskyblue color
    # 添加蓝色条形，stat表示数据统计方式，也就是说identity提取横坐标x对应的y值
    geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +

    # The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    # 设置y的范围，负值设定内圆的大小，正值设定各个条柱的最高高度
    ylim(-100,120)+

    # theme_minimal简约主题
    theme_minimal() +
    # Custom the theme: no axis title and no cartesian grid
    # 自定义主题
    theme(
        # 移除标题坐标文字
        axis.text = element_blank(),
        axis.title = element_blank(),
        # 移除网格
        panel.grid = element_blank(),
        # This remove unnecessary margin around plot
        # 移除不必要空白
        plot.margin = unit(rep(-2,4), "cm"))+

    # This makes the coordinate polar instead of
    # 设置极坐标系
    coord_polar(start = 0) +

    # Add the labels, using the label_data dataframe that we have created before
    # 添加标签
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

# library
library(tidyverse)
 
# Create dataset
# 添加数据
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)
 
# Set a number of 'empty bar'
# 设置空白柱的个数
empty_bar <- 10
 
# 在原始数据中添加空白数据
# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
# 和上一步一样，获得标签角度信息
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar   
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
head(label_data)

# Make the plot
# 绘图
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("green", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
 
p;

# library
library(tidyverse)
 
# Create dataset
# 创建数据集
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
# 在原始数据中添加空白数据
# empty_bar 表示组之间的空白距离
empty_bar <- 4
# 每一组之间4个空白
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
# 为每个空白值提供组信息，rep函数的意思就是复制值，levels(data$group)为复制的对象，each为复制的次数
to_add$group <- rep(levels(data$group), each=empty_bar)
head(to_add)

colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
# 管道操作类似 data<-arrange(data,data$group)
data <- data %>% arrange(group)
# 设置id
data$id <- seq(1, nrow(data))
head(data)

# Get the name and the y position of each label
# 设定角度值
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# Make the plot
# fill 按组填充颜色
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +   
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

# library
library(tidyverse)
 
# Create dataset
# 创建数据集
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
# 在原始数据中添加空白数据
# empty_bar 表示组之间的空白距离
empty_bar <- 4
# 每一组之间4个空白
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
# 为每个空白值提供组信息，rep函数的意思就是复制值，levels(data$group)为复制的对象，each为复制的次数
to_add$group <- rep(levels(data$group), each=empty_bar)
head(to_add)

colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
# 管道操作类似 data<-arrange(data,data$group)
data <- data %>% arrange(group, value)
# 设置id
data$id <- seq(1, nrow(data))
head(data)

# Get the name and the y position of each label
# 设定角度值
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# Make the plot
# fill 按组填充颜色
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +   
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

# library
library(tidyverse)
 
# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)
 
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar   
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
head(label_data)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
head(base_data)

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
grid_data

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +   
    # 添加条形图
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

    # 添加各组之间的线条，可以注释
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines，设置值坐标，可以注释
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    # 和前面一样
    ylim(-100,120) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

    # Add base line information
    # 添加下划线
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    # 添加各组的名字
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p

# library
library(tidyverse)
library(viridis)
 
# Create dataset
# 创建数据集
data <- data.frame(
    individual=paste( "Mister ", seq(1,60), sep=""),
    group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
    value1=sample( seq(10,100), 60, replace=T),
    value2=sample( seq(10,100), 60, replace=T),
    value3=sample( seq(10,100), 60, replace=T)
)
head(data)

# Transform data in a tidy format (long format)
# key表示观察的变量就是value1,value2,value3;value代表值,-c(1,2)表示不对第一列和第二列进行转换
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 
head(data)
dim(data)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
 
# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
 
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
# 保存数据 Save at png
ggsave(p, file="output.png", width=10, height=10)
