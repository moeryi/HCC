# F2A
x=rbind(GSE14520,GSE25097,GSE76427,TCGA)
colnames(x)[1]="immunity.cel"
x$Type <- factor(ifelse(x$P.Value < 0.05 & abs(x$logFC) >=0,ifelse(x$logFC > 0 ,'UP','DOWN'),'NOT'),levels=c('UP','NOT','DOWN'))##这里颜色也可以根据值来做出设定的颜色
##普通版
pp=ggplot(x,aes(Set,immunity.cel))
pbubble=pp+geom_point(alpha = 0.8,aes(size=-log(P.Value,10),colour=Type,fill=Type))+
  scale_color_manual(values=c("red", "grey","blue"))###colour能够增加说明
pbubble
pr = pbubble+ labs(size="-log[10](FDR)",title = "DEG with immunity.cells in different sets", x = "Set",y = "immunity.cells")+ 
     theme_bw()+ #去除背景色
     theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="black", size=12),
        axis.text.y = element_text(face="bold",  color="black", size=12),
        axis.title.x = element_text(face="bold", color="black", size=12),
        axis.title.y = element_text(face="bold",color="black", size=12))
pr

##改进版
# 主要修改点
pbubble= pp+geom_point(stat= "identity",alpha = 0.7,aes(size=-log(P.Value,10),colour=Type,fill=Type))+ 
  guides(color=guide_legend(title="Type"))+
  # scale_size(range = c(1, 10),guide=FALSE)+##等比例缩放，但会少说明
  scale_size_continuous(range=c(9,10))+##用这个
  scale_size_area(max_size = 8, breaks=c(20,40,60,80,100)) + #设定点的大小比例和图例上显示的间隔
  scale_color_manual(values=c("red", "grey","blue"))###colour能够增加说明
  
pr = pbubble+ labs(size="-log[10](FDR)",title = "DEG with immunity.cells in different sets", x = "Set",y = "immunity.cells")+ 
     theme_bw()+ #去除背景色
     theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="black", size=12),
        axis.text.y = element_text(face="bold",  color="black", size=12),
        axis.title.x = element_text(face="bold", color="black", size=12),
        axis.title.y = element_text(face="bold",color="black", size=12))
pr