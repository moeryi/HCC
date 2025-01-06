# F3C
##GSEA折线图
library(ggplot2)
p4 <- ggplot(result,aes(x=RANK.IN.GENE.LIST,y=RUNNING.ES,colour=pathway,group=pathway))+
  geom_line(size = 1.5) + 
  scale_color_manual(values = mycol[1:nrow(result)]) + #用前面自定义的颜色
  
  labs(x = "", y = "Enrichment Score", title = "") + 
  scale_x_continuous(expand = c(0, 0)) + #让x和y轴都从0开始
  scale_y_continuous(expand = c(0, 0),
                     limits = c(min(result$RUNNING.ES - 0.02), max(result$RUNNING.ES + 0.02))) + 
  
  theme_bw() + #去除背景色
  theme(panel.grid = element_blank()) + #去除网格线
  theme(panel.border = element_blank()) + #去除外层边框
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank()) + #去除x轴
  geom_hline(yintercept = 0) + #在0的位置画x轴
  
  theme(legend.position = c(0.55,0.68),legend.justification = c(0,0)) + #legend画在右下角，移动位置
  guides(colour = guide_legend(title = NULL)) + #隐藏由color产生的图例的title
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())
p4
ggsave(file="line.pdf")






# F3D
# 有了节点和边的数据，使用 `tbl_graph()` 便可以得到一个图。
graph <- tbl_graph(nodes, edges)
# 开始画图
# 自定义配色，直接出图
# 用 `ggraph` 出图
gc <- ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
  # 使用 filter 参数去掉 root（前面设置为"all"）节点及与其相连的边
  geom_edge_diagonal(aes(color = node1.node.branch,##指定颜色可以看nodes文本的node1.node.branch列
                         filter=node1.node.level!="all"), 
                     alpha = 1/3,edge_width=1) + 
  geom_node_point(aes(size = node.size, 
                      color = node.branch,
                      filter=node.level!="all"), alpha = 1/3) + 
  scale_size(range = c(0.5,80)) + #做均一化处理，让点的大小介于range之间
  theme(legend.position = "none") + #不画图例
  
  # 点和边的配色
  # 如果要改变边的配色，需要同时给边和点变色，否则会对应不上
  scale_edge_color_brewer(palette = "Set1") + #用?scale_color_brewer查看更多配色方案
  scale_color_brewer(palette = "Set1") +
  
  # 添加周围注释文字，此处是基因名gene
  geom_node_text(
    aes(
      x = 1.048 * x, #控制字跟点的距离
      y = 1.048 * y, #控制字跟点的距离
      label = node.short_name,
      angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      filter = leaf,
      color = node.branch
      ),
    size = 6, hjust = 'outward') +
  
  # 添加内环文字，此处是通路名term
  geom_node_text(
    aes(label=node.short_name,
        filter = !leaf & (node.level != "all"),
        color = node.branch),
    fontface="bold",
    size=6,
    family="sans"
  ) + 
  theme(panel.background = element_rect(fill = NA)) +
  coord_cartesian(xlim=c(-1.3,1.3),ylim = c(-1.3,1.3)) #扩大坐标系

gc
ggsave("ccgraph_cor.pdf",width = 20,height = 16)