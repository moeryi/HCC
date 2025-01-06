# F1A
# 开始画图
table(data$type)
mycol <- c("TCGA_Up" = "#223D6C", "TCGA_Down" = "#D20A13","TCGA_Not" = "#D3D3D3","GSE14520_Up" = "#FFD121","GSE14520_Down" = "#088247",
"GSE14520_Not" = "#D3D3D3","GSE25097_Up" ="#11AA4D","GSE25097_Down" ="#58CDD9","GSE25097_Not" = "#D3D3D3",
"GSE76427_Up" ="#7A142C","GSE76427_Down" ="#5D90BA","GSE76427_Not" = "#D3D3D3")  

library("ggplot2")
p <- ggplot(data = data, mapping = aes(x = order, y = P.Value)) + 
  geom_point(aes(colour = type, #每条染色体用不同的颜色
                 shape = set, #不同类型用不同形状
                 size = pcut)) + #阈值上下的点用不同大小
  #如果没有分类信息，或者不需要用不同的形状显示分类，就运行下面这行
  #geom_point(aes(colour = CHR)) +
  scale_size_discrete(range = c(0.5,2)) + #点的大小范围
  #scale_shape_manual(values = c(20,8,15)) + #点的形状
  
  #添加阈值的红色直线
  # geom_hline(yintercept = logPcutoff, color = 'red') +
  
  xlab("") + ylab(expression(-log[10](P))) + 
  scale_x_continuous(##label = axisdf$chr, breaks= axisdf$center,
                     expand = expand_scale(mult = c(0,0.02))) + #左边到头，右边留空
  scale_y_continuous(expand = expand_scale(mult = c(0,0.04))) + #下面到头，上面留空
  
  theme_bw() + #去除背景色
  theme(panel.grid =element_blank()) + #去除网格线
  theme(panel.border = element_blank()) + #去除外层边框
  theme(axis.text = element_text(size = rel(0.8)), #坐标轴标签字号
        axis.line = element_line(colour = "black")) #沿坐标轴显示直线

p1 <- p + scale_color_manual(values = mycol) + #使用自定义的颜色
  guides(colour = FALSE, size = FALSE) + #不显示染色体图例
  theme(legend.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8), #图例字号
        legend.position = c(1,1),legend.justification = c(1,1)) #形状的图例放在右上角
p1



#图例放到底部
p2 <- p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +#去除x轴刻度和标签
  guides(color = guide_legend(nrow = 2), #染色体的图例排两行
         size = FALSE) + #不显示点大小的图例
  theme(legend.background = element_blank(), 
        legend.title = element_blank(),
        legend.box = "vertical", #两个图例垂直摆放
        legend.margin=margin(t= 0, unit='cm'), #图例不留边
        legend.spacing = unit(0,"in"), #两个图例紧挨着
        legend.position = "bottom") + #图例放在底部
  scale_color_manual(labels = paste0("Type_",unique(data$type)),
                     values = mycol)
p2

# 再加箭头和标签
library(ggrepel)
p2 + geom_text_repel(aes(label = label), 
                     size = 2, #标签文字的字号
                     point.padding = unit(0.5, "lines"), # label与point之间的连接显示
                     segment.colour = "black", # 连接（直线＋箭头）的颜色
                     segment.size = 0.4, # 连接（直线＋箭头）的粗细
                     arrow = arrow(angle = 20, length = unit(0.04,"inches")), # 箭头设置参考?arrow
                     ylim = c(logPcutoff, max(data$P)), # 限制label的绘图区域
                     direction = "both", # 默认在x、y轴方向上调整label的位置
                     min.segment.length = 0, #默认长度小于0.5的箭头就不画了
                     na.rm = TRUE #不提示移除NA
                     )

#保存到pdf文件
ggsave("Manhattan_deg.pdf",width = 7,height = 4)
