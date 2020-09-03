
# 调用ggplot2库
library(ggplot2)
# 展示金刚石数据集
head(diamonds)

# if only the dataset is known. 只显示数据
ggplot(diamonds)

# if only X-axis is known. The Y-axis can be specified in respective geoms.
# 只设定x轴，y轴数据可以在geoms中指定
ggplot(diamonds, aes(x=carat))  

# if both X and Y axes are fixed for all layers.
# 指定x轴和y轴
ggplot(diamonds, aes(x=carat, y=price))  

# Each category of the 'cut' variable will now have a distinct color, once a geom is added.
# 指定颜色类别cut
ggplot(diamonds, aes(x=carat, color=cut)) 

ggplot(diamonds, aes(x=carat), color="steelblue")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
# Adding scatterplot geom (layer1) 添加散点图
geom_point() + 
# Adding moothing geom (layer2) 在散点图的基础上添加一条平滑的趋势曲线
geom_smooth() 

 # Same as above but specifying the aesthetics inside the geoms. 类似上面的结果
ggplot(diamonds) + 
geom_point(aes(x=carat, y=price, color=cut)) + 
geom_smooth(aes(x=carat, y=price, color=cut))

library(ggplot2)
ggplot(diamonds) + 
geom_point(aes(x=carat, y=price, color=cut)) + 
# Remove color from geom_smooth 只画一条拟合平滑线
geom_smooth(aes(x=carat, y=price)) 

 # same but simpler 类似上图同样的功能
ggplot(diamonds, aes(x=carat, y=price)) + 
geom_point(aes(color=cut)) + 
geom_smooth() 

# Answer to the challenge 设置形状点
ggplot(diamonds, aes(x=carat, y=price, color=cut, shape=color)) + 
geom_point()

gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
# add axis lables and plot title 添加标签
labs(title="Scatterplot", x="Carat", y="Price")
print(gg)

gg1 <- gg + 
    theme(
    # 设置标题大小，face="bold"字体加粗
        plot.title=element_text(size=30, face="bold"), 
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25)) +
    # add title and axis text, change legend title.
    # 添加渐变色，并设置颜色条图例标题
    scale_color_discrete(name="Cut of diamonds")  
print(gg1)  # print the plot

gg1 + facet_wrap( ~cut , ncol=3)

# row: color, column: cut
gg1 + facet_wrap(color ~ cut)  

# row: color, column: cut
# gg1 + facet_wrap(color ~ cut, scales="free")

gg1 + facet_grid(color ~ cut) 

# 载入库
library(ggfortify)
# 查看数据
AirPassengers

autoplot(AirPassengers) + 
labs(title="AirPassengers")

# Approach 1:
data(economics, package="ggplot2")  # init data
economics <- data.frame(economics)  # convert to dataframe
# 展示数据
head(economics)

# 画图
ggplot(economics) + 
# 画线条
geom_line(aes(x=date, y=pce, color="pcs")) + 
geom_line(aes(x=date, y=unemploy, col="unemploy")) + 
# 设定颜色
scale_color_discrete(name="Legend") + 
labs(title="Economics")

# Approach 2:
library(reshape2)
# 融合数据
df <- melt(economics[, c("date", "pce", "unemploy")], id="date")
head(df)

# 绘图
ggplot(df) + 
geom_line(aes(x=date, y=value, color=variable)) + 
labs(title="Economics")

df <- melt(economics[, c("date", "pce", "unemploy", "psavert")], id="date")
ggplot(df) + 
geom_line(aes(x=date, y=value, color=variable)) +
facet_wrap( ~ variable, scales="free")

# 显示数据
head(mtcars)
plot1 <- ggplot(mtcars, aes(x=cyl)) + 
# 画柱状图
geom_bar() + 
# Y axis derived from counts of X item
labs(title="Frequency bar chart")  
print(plot1)

df <- data.frame(var=c("a", "b", "c"), nums=c(1:3))
# 显示数据
df
# Y axis is explicit. 'stat=identit
# 显示y
plot2 <- ggplot(df, aes(x=var, y=nums)) + 
geom_bar(stat = "identity")
print(plot2)

library(gridExtra)
# 分配图像
grid.arrange(plot1, plot2, ncol=2)

df <- data.frame(var=c("a", "b", "c"), nums=c(1:3))
ggplot(df, aes(x=var, y=nums)) + 
geom_bar(stat = "identity") + 
# 翻转坐标轴
coord_flip() + 
labs(title="Coordinates are flipped")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() + 
# 设置y轴范围
coord_cartesian(ylim=c(0, 10000)) + 
labs(title="Coord_cartesian zoomed in!")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() + 
# 设定范围
ylim(c(0, 10000)) + 
labs(title="Datapoints deleted: Note the change in smoothing lines!")

ggplot(diamonds, aes(x=price, y=price+runif(nrow(diamonds), 100, 10000), color=cut)) + 
geom_point() + 
geom_smooth() + 
coord_equal()

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() +
# 更改主题
theme_bw() + 
labs(title="bw Theme")

# 无图例
p1 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() + 
# 无图例
theme(legend.position="none") + 
labs(title="legend.position='none'")

p2 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() + 
# legend at top 设置图例在图形顶部
theme(legend.position="top") + 
labs(title="legend.position='top'")  

p3 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
geom_point() + 
geom_smooth() + 
labs(title="legend.position='coords inside plot'") + 
# legend inside the plot 设置图形位置
theme(legend.justification=c(1,0), legend.position=c(1,0))  

# arrange统一显示图像
grid.arrange(p1, p2, p3, ncol=3)  

ggplot(mtcars, aes(x=cyl)) + 
geom_bar(fill='darkgoldenrod2') +
theme(panel.background = element_rect(fill = 'steelblue'),
# 设置主网格线
panel.grid.major = element_line(colour = "firebrick", size=3),
panel.grid.minor = element_line(colour = "blue", size=1))

ggplot(mtcars, aes(x=cyl)) + 
geom_bar(fill="firebrick") + 
# top, right, bottom, left
# plot.background设置背景，plot.margain设置边距
theme(plot.background=element_rect(fill="steelblue"), plot.margin = unit(c(2, 4, 1, 3), "cm")) 

library(grid)
# 添加注释
my_grob = grobTree(textGrob("This text is at x=0.1 and y=0.9, relative!\n Anchor point is at 0,0", x=0.1,  y=0.9, hjust=0,
  gp=gpar(col="firebrick", fontsize=25, fontface="bold")))
ggplot(mtcars, aes(x=cyl)) + 
geom_bar() + 
annotation_custom(my_grob) + 
labs(title="Annotation Example")

plot1 <- ggplot(mtcars, aes(x=cyl)) + 
geom_bar()
# 保存图像
ggsave("myggplot.png")  # saves the last plot.
ggsave("myggplot.png", plot=plot1)  # save a stored ggplot
