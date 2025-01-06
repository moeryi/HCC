# F2E
##平滑曲线型
# mycol <- c("slateTurquoise1","seagreen3","dodgerTurquoise1","firebrick1","lightgoldenrod","Turquoise1","orange2")##设定颜色
pdf("AUC.pdf",height=6,width=6)
plot.roc(probe_group_EXP$Group,probe_group_EXP[,2],xlim=c(1,0), smooth=T, #绘制平滑曲线
ci=TRUE, main= "AUC",#print.thres="best", #把阈值写在图上，其sensitivity+ specificity之和最大
col="#431A3D",#线的颜色
lwd=3, #线的粗细
legacy.axes=T)
mycol <- rep(c("#223D6C","#D20A13","#FFD121","#5D90BA","#11AA4D","#58CDD9","#7A142C","#431A3D"))
##循环增加线
for (i in 3:9){
        plot.roc(probe_group_EXP$Group,probe_group_EXP[,i],xlim=c(1,0), smooth=T, #绘制平滑曲线
        ci=TRUE,#print.thres="best", #把阈值写在图上，其sensitivity+ specificity之和最大
        col=mycol[i-2],#线的颜色
        lwd=3, #线的粗细
        legacy.axes=T,add=T)
    }
##贴标签
legend("bottomright", legend=c(paste("BOP1, AUC=", signif(AUC[1,2], digits = 2), sep=""), 
paste("BUB1B, AUC=",signif(AUC[2,2], digits = 2), sep=""),
paste("ECM1, AUC=",signif(AUC[3,2], digits = 2), sep=""),
paste("HCLS1, AUC=",signif(AUC[4,2], digits = 2), sep=""),
paste("NOTCH3, AUC=",signif(AUC[5,2], digits = 2), sep=""),
paste("PCK2, AUC=",signif(AUC[6,2], digits = 2), sep=""),
paste("SCAMP3, AUC=",signif(AUC[7,2], digits = 2), sep=""),
paste("SNRPD2, AUC=",signif(AUC[8,2], digits = 2), sep="")),
       col=c("#431A3D","#223D6C","#D20A13","#FFD121","#5D90BA","#11AA4D","#58CDD9","#7A142C"), lwd=0.7,cex=1.1)##贴标签,digits = 2小数点位数
dev.off()	
