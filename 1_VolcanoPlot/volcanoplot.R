
# 1. 加载所需R包
library(ggplot2)      # 数据可视化
library(ggrepel)      # 自动调整标签位置
library(RColorBrewer) # 调色板
library(grid)         # 网格图形系统
library(scales)       # 图形缩放函数

# 2. 读取数据
rm(list=ls())  # 清除全局环境
setwd('C:\\Users\\31277\\Desktop\\公众号资料\\火山图2')  # 设置工作目录

# 3. 读取数据
df <- read.table(file="data.txt", sep="\t", header=TRUE, check.names=FALSE)

# 4. 根据padj和log2FoldChange值分类：上调、下调、不显著
df$group <- as.factor(ifelse(df$padj < 0.05 & abs(df$log2FoldChange) >= 2, 
                             ifelse(df$log2FoldChange >= 2, 'up', 'down'), 'NS'))

# 5. 标记需要显示基因名的点（padj<0.05且|log2FoldChange|>=4）
df$label <- ifelse(df$padj < 0.05 & abs(df$log2FoldChange) >= 4, as.character(df$gene), '')

p <- ggplot(df, aes(log2FoldChange, -log10(padj), fill=group)) +
  geom_point(color="black", alpha=0.6, size=3, shape=21) +  # 散点
  theme_bw() +  # 黑白主题
  theme(panel.grid=element_blank(),  # 去除网格线
        axis.text=element_text(color='#333c41', size=10),
        legend.text=element_text(color='#333c41', size=10),
        legend.title=element_blank(),
        axis.title=element_text(size=12)) +
  geom_vline(xintercept=c(-2, 2), lty=3, color='black', lwd=0.8) +  # log2FC阈值线
  geom_vline(xintercept=c(-4, 4), lty=3, color='red', lwd=0.8) +    # 强差异表达阈值线
  geom_hline(yintercept=-log10(0.05), lty=3, color='black', lwd=0.8) +  # padj阈值线
  scale_fill_manual(values=c('blue', 'grey', 'red')) +  # 自定义颜色
  labs(title="volcanoplot", x='log2 fold change', y='-log10 padj') +  # 标题和轴标签
  geom_text_repel(aes(x=log2FoldChange, y=-log10(padj), label=label),  # 添加基因标签
                  max.overlaps=10000, size=3, box.padding=unit(0.8,'lines'),
                  point.padding=unit(0.8, 'lines'), segment.color='black',
                  show.legend=FALSE)
print(p)

# 7. 添加背景色渐变
color <- colorRampPalette(brewer.pal(11, "BrBG"))(30)  # 生成30种颜色
grid.raster(alpha(color, 0.2), width=unit(1, "npc"), height=unit(1, "npc"), interpolate=TRUE)
