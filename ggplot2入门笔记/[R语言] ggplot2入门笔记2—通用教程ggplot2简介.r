
# Setup
# #关闭科学记数法，如1e+06
# turn off scientific notation like 1e+06
options(scipen=999)  
library(ggplot2)
# load the data 载入数据
data("midwest", package = "ggplot2")
# 显示数据
head(midwest)
# Init Ggplot 初始化图像
# area and poptotal are columns in 'midwest'
ggplot(midwest, aes(x=area, y=poptotal))  

library(ggplot2)
ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point()

g <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point() + 
# set se=FALSE to turnoff confidence bands
# 设置se=FALSE来关闭置信区间
geom_smooth(method="lm", se=TRUE)  
plot(g)

library(ggplot2)
# set se=FALSE to turnoff confidence bands
# 设置se=FALSE来关闭置信区间
g <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point() + 
geom_smooth(method="lm")

# Delete the points outside the limits
# deletes points 删除点
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))
# g + xlim(0, 0.1) + ylim(0, 1000000)   

library(ggplot2)
g <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point() + 
# set se=FALSE to turnoff confidence bands
geom_smooth(method="lm")  

# Zoom in without deleting the points outside the limits. 
# As a result, the line of best fit is the same as the original plot.
# 放大而不删除超出限制的点。因此，最佳拟合线与原始图相同。
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  
plot(g1)

library(ggplot2)
# 画图
# set se=FALSE to turnoff confidence bands
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  
# 限制范围
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
# Add Title and Labels
# 添加标签，标题名，小标题名，说明文字
g1 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# 另外一种方法
g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")

# Full Plot call
library(ggplot2)
ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point() + 
geom_smooth(method="lm") + 
coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

library(ggplot2)
# 画图
ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set static color and size for points
# 设置固定颜色和尺寸
geom_point(col="steelblue", size=3) +   
# change the color of line
# 更改拟合直线颜色
geom_smooth(method="lm", col="firebrick") +  
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

library(ggplot2)
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories.
# 根据状态类别将颜色设置为不同。
geom_point(aes(col=state), size=3) +  
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)

# remove legend 移除图例
gg + theme(legend.position="None")  

# change color palette 更改调色板
gg + scale_colour_brewer(palette = "Set1")  

library(RColorBrewer)
head(brewer.pal.info, 10)

library(ggplot2)

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories
# 设置颜色
geom_point(aes(col=state), size=3) + 
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change breaks
# 改变间距
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))

library(ggplot2)

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories
# 设置颜色
geom_point(aes(col=state), size=3) + 
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change breaks + label
# letters字母表
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

library(ggplot2)

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories
# 设置颜色
geom_point(aes(col=state), size=3) + 
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Reverse X Axis Scale
# 反转x轴
gg + scale_x_reverse()

library(ggplot2)

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories
# 设置颜色
geom_point(aes(col=state), size=3) + 
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change Axis Texts
gg + 
# 更改x轴
scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
# 更改y轴
scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})

library(ggplot2)

# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
# Set color to vary based on state categories.
geom_point(aes(col=state), size=3) +  
geom_smooth(method="lm", col="firebrick", size=2) + 
coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))

# method 1: Using theme_set()
theme_set(theme_classic())  
gg

# method 2: Adding theme Layer itself.
# 添加主题层
gg + theme_bw() + labs(subtitle="BW Theme")

gg + theme_classic() + labs(subtitle="Classic Theme")
