# F2D
# 开始画图
# 使用ggplot2画图，主要用到的是geom_tile函数：
# 相关性用颜色的不同来表示，相关性的大小用颜色的深浅来反映；
# 有差异的把*号打印在热图上
library(ggplot2)
## Warning: package 'ggplot2' was built under R version 3.5.2
library(dplyr)
ggplot(data, aes(immune_cells, gene)) + 
  geom_tile(aes(fill = desc(-cor)), colour = "white",size=1)+
  scale_fill_gradient2(low = "#2b8cbe",mid = "white",high = "#e41a1c")+
  geom_text(aes(label=pstar),col ="black",size = 5)+
  theme_minimal()+# 不要背景
  theme(axis.title.x=element_blank(),#不要title
        axis.ticks.x=element_blank(),#不要x轴
        axis.title.y=element_blank(),#不要y轴
        axis.text.x = element_text(angle = 45, hjust = 1),# 调整x轴文字
        axis.text.y = element_text(size = 8))+#调整y轴文字
  #调整legen
  labs(fill =paste0(" * p < 0.05","\n\n","** p < 0.01","\n\n","Correlation"))