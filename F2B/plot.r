# F2B
# 开始画图
# 用complexheatmap画图。complexheatmap的参数众多，有兴趣了解的详情https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html
library(circlize)
library(ComplexHeatmap)
heatmapinput <- read.csv("heatmapinput.csv", row.names = 1)
ml <- heatmapinput[, c(2:25)] #画图的ssGSEA数据
ml <- as.data.frame(t(apply(ml, 2, scale))) #scale标化
colnames(ml) <- rownames(heatmapinput)
col_fun <- colorRamp2(c(-5, 0, 5), c("#377EB8", "white", "#E41A1C"))##设置热图颜色区域
#获取聚类分组信息
h1 <- Heatmap(ml, cluster_rows = TRUE, cluster_columns = TRUE, clustering_method_columns = "ward.D2",show_row_names = TRUE, show_column_names = FALSE,
              clustering_distance_columns = "euclidean", 
              clustering_distance_rows = "euclidean",
              clustering_method_rows  = "ward.D2")##外部聚类参数可选
tree <- column_dend(h1)
ind <- cutree(as.hclust(tree), k = 2)[order.dendrogram(tree)] #此处划分为2类，根据你自己的数据调整
table(ind)
## ind
##   1   2 
## 110 259  
heatmapinput$Immune_infiltration <- ind[heatmapinput$barcode]##增加分类模块信息
draw(h1)
heatmapinput$Immune_infiltration <- str_replace(heatmapinput$Immune_infiltration, "1", "Low infiltration")##替换字符
heatmapinput$Immune_infiltration <- str_replace(heatmapinput$Immune_infiltration, "2", "High infiltration")
write.csv(heatmapinput, "sample.csv")
#临床信息注释
head(heatmapinput[,26:33])
table(heatmapinput$stage)
heatmapinput$stage =gsub("--", " ", heatmapinput$stage)##缺失信息填充空
table(heatmapinput$event)
table(heatmapinput$days)
heatmapinput$days =gsub("--", "0", heatmapinput$days)##缺失信息填充空
Immune_infiltration <- heatmapinput[, "Immune_infiltration"]
# Gender <- heatmapinput[, "gender"]
Survival <- heatmapinput[, "event"] #这种生存信息是没有什么价值的，建议用生存状态加时间，详细画法可以参考complexheatmap
Stage <- heatmapinput[, "stage"]
ha = HeatmapAnnotation(Immune_infiltration =Immune_infiltration,
                       Survival = Survival, Stage = Stage,
                       col = list(Immune_infiltration = c("High infiltration" = "#3FA538", "Low infiltration" = "#9FD29BFF"),
                                  # Gender = c("male" = "#C4868E", "female" = "#97A8C7"),
								  Survival = c("Alive" = "#3FA538", "Dead" = "#E00115", "Not Reported" = "#97A8C7"),
                                  Stage =  c("Stage I" = "#B0B0FFFF",  "Stage II" = "#B0FFB0FF" , 
                                             "Stage III" = "#F7E897FF","Stage IV" = "#FF6060FF")),na_col = "white",#各个临床信息的颜色，Anatomic_location颜色输入也同其他，na_col = "white"，缺失值为白色
                       show_legend = rep(TRUE, 12),#是否要显示annotation legend
                       annotation_height = unit(rep(5, 12), "mm"),#临床annotation的高度
                       annotation_legend_param = list(
                         Immune_infiltration = list(title = "Immune infiltration"),
                         # Gender = list(title = "Gender"),
                         Survival = list(title = "Survival"),
                         Stage = list(title = "Stage"))#annotation legend的标签
)

top_annotation <- HeatmapAnnotation(days = anno_points(as.numeric(heatmapinput$days),axis_param =  # 年龄注释
                                                  list(side = "left",
                                                       at = c(0, 1825, 2920),
                                                       labels = c("0","1825","2920"))),###年龄范围
                              height = unit(2.7, "cm"),
                              show_annotation_name = T)



ht <- Heatmap(ml, col = col_fun, 
              name = "LIHC ssGSEA",
              cluster_rows = TRUE, cluster_columns = TRUE, top_annotation =top_annotation,         
              show_row_names = TRUE, show_column_names = FALSE,
              bottom_annotation = ha, column_title = qq("TCGA LIHC samples (n = @{ncol(ml)})"),
              clustering_method_columns = "ward.D2",
              clustering_distance_columns = "euclidean", 
              clustering_distance_rows = "euclidean",
              clustering_method_rows  = "ward.D2", column_dend_height = unit(30, "mm")
              )
pdf("ssGSEA.pdf", 16, 12)
draw(ht, annotation_legend_side = "left", heatmap_legend_side = "left")

#装饰heatmap
annotation_titles <- c(Immune_infiltration = "Immune infiltration",
                      # Gender = "Gender",
                      Survival = "Survival",
                      Stage = "Stage")
for(an in names(annotation_titles)) {
  decorate_annotation(an, {
    grid.text(annotation_titles[an], unit(-2, "mm"), just = "right")#对齐方向是右边
    grid.rect(gp = gpar(fill = NA, col = "black"))
  })
}

#分组划线：具体数值要参考聚类的信息##根据ind（分组调整位置）
decorate_heatmap_body("LIHC ssGSEA", {
  grid.lines(c(0, 0), c(0, 1), gp = gpar(lty = 1, lwd = 2))
  grid.lines(c(table(ind)[[2]]/sum(table(ind)), table(ind)[[2]]/sum(table(ind))), 
             gp = gpar(lty = 2, lwd = 2))
  grid.lines(c(1, 1), c(0, 1), gp = gpar(lty = 1, lwd = 2))
}, slice = 1)##slice = 1为行2为列

#在heatmap上注释文字信息
decorate_heatmap_body("LIHC ssGSEA", {
    grid.text("Low infiltration", (table(ind)[[1]]/0.35)/sum(table(ind)), 0.1, #文字所放位置的x和y，根据自己的数据调整
              default.units = "npc", gp = gpar(fontsize = 16))
    grid.text("High infiltration", (1-(table(ind)[[1]] + table(ind)[[2]]/2)/sum(table(ind))), 0.1, default.units = "npc", gp = gpar(fontsize = 16))
}, slice = 1)

dev.off()
## quartz_off_screen 
##                 2
sessionInfo()