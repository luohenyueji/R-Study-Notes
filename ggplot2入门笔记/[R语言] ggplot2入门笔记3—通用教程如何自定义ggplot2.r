
# Setup
options(scipen=999)
library(ggplot2)
data("midwest", package = "ggplot2")
theme_set(theme_bw())

# Add plot components --------------------------------
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Call plot ------------------------------------------
plot(gg)

library(ggplot2)

# Base Plot 基础绘图
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

library(showtext)
showtext.auto(enable = TRUE)
# 添加字体
font.add('SimSun', 'simsun.ttc')

# Modify theme components 
# 修改主题
gg + theme(
    # 设置标题
    plot.title=element_text(size=20, # 字体大小
                            face="bold", # 字体加粗
                            family="SimSun", # 字体类型
                            color="tomato", # 字体颜色
                            hjust=0.5, # 标题离左边距距离
                            lineheight=1.2),  # 线条高度
    # 设置子标题
    plot.subtitle=element_text(size=15, # 字体大小
                               family="SimSun", # 字体类型
                               face="bold", # 字体加粗
                               hjust=0.5),  # 标题离左边距距离
    # caption 注释
    plot.caption=element_text(size=15),  
    # X axis title X轴标题
    axis.title.x=element_text(vjust=0,
                              size=15), 
    # Y axis title Y轴标题
    axis.title.y=element_text(size=15),
    # X axis text X轴文字
    axis.text.x=element_text(size=10,
                             angle = 30,
                             vjust=.5),
    # Y axis text Y轴文字
    axis.text.y=element_text(size=10))  

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# modify legend title
# 单独调用labs修改颜色和字体
gg + labs(color="State", size="Density")  

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# modify legend title
# 修改legend
gg <- gg + guides(color=guide_legend("State"), size=guide_legend("Density"))
plot(gg)

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Modify Legend 修改图例
# guide = FALSE turn off legend for size 关闭size的图例
# scale_color_discrete(name="States") 设置离散颜色变量的图例
gg + scale_color_discrete(name="States") + scale_size_continuous(name = "Density", guide = FALSE)  

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

gg + scale_color_manual(name="State", 
                        # 设置标签
                        labels = c("Illinois", 
                                   "Indiana", 
                                   "Michigan", 
                                   "Ohio", 
                                   "Wisconsin"), 
                        # 设置标签对应的颜色
                        values = c("IL"="blue", 
                                   "IN"="red", 
                                   "MI"="green", 
                                   "OH"="brown", 
                                   "WI"="orange"))

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# order设置位置顺序
gg + guides(colour = guide_legend(order = 2), size = guide_legend(order = 1))

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

gg + theme(
    # 设置图例标题字体颜色和大小
    legend.title = element_text(size=12, color = "firebrick"), 
    # 设置图例内容文字大小
    legend.text = element_text(size=10),
    # 设置背景色
    legend.key=element_rect(fill='springgreen')) +
# 设置内部图例圆圈大小和间距
guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# No legend 
# 无图例
gg + theme(legend.position="None") + labs(subtitle="No Legend")

# Legend to the left 图例位置在左边
gg + theme(legend.position="left") + labs(subtitle="Legend on the Left")

# legend at the bottom and horizontal
# 图例位于图像底部，图例水平摆放
gg + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="Legend at Bottom")

# legend at bottom-right, inside the plot
# 图例位于图像内部右下角
gg + theme(
    # 设置图像标题
    legend.title = element_text(size=12, color = "salmon", face="bold"),
    # 设置图像铰点为图内左下角
    legend.justification=c(1,0), 
    # 图例位置
    legend.position=c(0.95, 0.05), 
    # 图例背景
    legend.background = element_blank(),
    # 图例填充颜色
    legend.key = element_blank()) + 
labs(subtitle="Legend: Bottom-Right Inside the Plot")

# legend at top-left, inside the plot 
# 图例位于图像内部左上角
gg + theme(
    # 设置标题名
    legend.title = element_text(size=12, color = "salmon", face="bold"),
    # 设置图像铰点为图内右上角
    legend.justification=c(0,1), 
    legend.position=c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank()) + 
labs(subtitle="Legend: Top-Left Inside the Plot")

library(ggplot2)

# Filter required rows.
# 获取数据
midwest_sub <- midwest[midwest$poptotal > 300000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 300000, midwest_sub$county, "")

# Base Plot
# 基础绘图
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Plot text and label
# 添加标签
gg + geom_text(aes(label=large_county), size=2, data=midwest_sub) + 
# 小标题
labs(subtitle="With ggplot2::geom_text") + 
# 无图例
theme(legend.position = "None")

# 添加标签和透明度
gg + geom_label(aes(label=large_county), size=2, data=midwest_sub, alpha=0.25) + 
labs(subtitle="With ggplot2::geom_label") + 
theme(legend.position = "None")

library(ggrepel)
# 调用ggrepel库添加标签
gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub) + 
labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None")

gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub) + 
labs(subtitle="With ggrepel::geom_label_repel") + theme(legend.position = "None")   # label

library(ggplot2)

# Base Plot
# 基础绘图
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Define and add annotation
library(grid)
#文本
my_text <- "This text is at x=0.7 and y=0.8!"
#my_grob = grid.text(my_text, x=0.7 and y=0.8, gp=gpar(col="firebrick", fontsize=14, fontface="bold"))
#gg + annotation_custom(my_grob)

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest", subtitle="X and Y axis Flipped") + 
theme(legend.position = "None")

# Flip the X and Y axis -------------------------------------------------
# 翻转X和Y轴
gg + coord_flip()

library(ggplot2)

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
geom_point(aes(col=state, size=popdensity)) + 
geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest", subtitle="Axis Scales Reversed") + 
theme(legend.position = "None")

# Reverse the X and Y Axis ---------------------------
# 反转X轴和Y轴
gg + scale_x_reverse() + scale_y_reverse()

library(ggplot2)
# 载入数据
data(mpg, package="ggplot2")
# 展示数据
head(mpg)
# 画图
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
geom_point() + 
labs(title="hwy vs displ", caption = "Source: mpg") +
geom_smooth(method="lm", se=FALSE) + 
theme_bw()
plot(g)

library(ggplot2)

# Base Plot
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
      geom_point() + 
      geom_smooth(method="lm", se=FALSE) + 
      theme_bw()

# Facet wrap with common scales
# 分面
# 以为class为列，分为3行
g + facet_wrap( ~ class, nrow=3) + 
# 共享标尺
labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure")

# Facet wrap with free scales
# 以列作为分块
g + facet_wrap( ~ class, scales = "free") + 
labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")

library(ggplot2)

# Base Plot
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
geom_point() + 
labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure") +
geom_smooth(method="lm", se=FALSE) + 
theme_bw()
# Add Facet Grid
# manufacturer in rows and class in columns
# 添加分面，列为class，行为manufacturer
g1 <- g + facet_grid(manufacturer ~ class)  
plot(g1)

library(ggplot2)

# Base Plot
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
geom_point() + 
geom_smooth(method="lm", se=FALSE) + 
labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Facet Grid - Multiple plots in one figure") +
theme_bw()

# Add Facet Grid
# cyl in rows and class in columns.
g2 <- g + facet_grid(cyl ~ class)  
plot(g2)

# Draw Multiple plots in same figure.
library(gridExtra)
gridExtra::grid.arrange(g1, g2, ncol=2)

library(ggplot2)

# Base Plot
# 基础绘图
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
geom_point() + 
geom_smooth(method="lm", se=FALSE) + 
theme_bw()  
g

# Change Plot Background elements
# 改变图像背景
g + theme(
    # 设置背景色
    panel.background = element_rect(fill = 'khaki'),
    # 设置图像网格主间隔
    panel.grid.major = element_line(colour = "burlywood", size=1.5),
    # 设置图像网格次间隔
    panel.grid.minor = element_line(colour = "tomato", size=.25, linetype = "dashed"),
    # 设置图像边缘
    panel.border = element_blank(),
    # x轴颜色宽度
    axis.line.x = element_line(colour = "darkorange", size=1.5, lineend = "butt"),
    # y轴颜色宽度
    axis.line.y = element_line(colour = "darkorange", size=1.5)) +
labs(title="Modified Background", subtitle="How to Change Major and Minor grid, Axis Lines, No Border")

# Change Plot Margins 
g + theme(plot.background=element_rect(fill="salmon"), 
          # top, right, bottom, left
          # 设置图像边缘
          plot.margin = unit(c(2, 2, 1, 1), "cm")) +
labs(title="Modified Background", subtitle="How to Change Plot Margin")  

library(ggplot2)

# Base Plot
# 基础绘图
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
geom_point() + 
geom_smooth(method="lm", se=FALSE) + 
theme_bw()

g + theme(
    # 主网格空白
    panel.grid.major = element_blank(), 
    # 次网格空白
    panel.grid.minor = element_blank(), 
    # 边缘空白
    panel.border = element_blank(),
    # 标题空白
    axis.title = element_blank(), 
    # 轴文字空白
    axis.text = element_blank(),
    axis.ticks = element_blank()) +

labs(title="Modified Background", subtitle="How to remove major and minor axis grid, border, axis title, text and ticks") 
