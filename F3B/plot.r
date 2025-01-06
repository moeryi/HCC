# F3B
library(cowplot)
library(ggplot2)
# 构造数据
#数据
# 气泡图部分绘制
#气泡图#BP_QI
k<-ggplot(x,aes(x=Description,y=Module,col=-log(pvalue,2),size=Count))+
  theme(legend.position = "bottom", plot.background = element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill='white', colour='gray'),
        panel.grid.major = element_line(size=1,linetype =3,colour = "gray"),
        strip.text.x=element_text(size=rel(1.2), family="serif", angle=-90),
        strip.text.y=element_text(size=rel(1.2),  family="serif", angle=-90) ,
		axis.text.x = element_text(face="bold", color="black", size=20, hjust = 1),##转置x轴坐标
        axis.text.y = element_text(face="bold",  color="black", size=15),
        axis.title.x = element_text(face="bold", color="black", size=15),
        axis.title.y = element_text(face="bold",color="black", size=15)
  )+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_colour_gradient(low = "#0099FF", high = "#CC00CC")+scale_size_continuous(range=c(5,10))+
 labs(color = expression(-log[10](FDR)),size="Count", x = "Description",y ="Module" )

k
ggsave(k, filename="BP_QI.pdf", width=12, height=16, units=c("cm"))
###条形图部分#BP_TIAO
pathway0 <- data.frame(x[,c(1,2)])
pathway0 <- aggregate(.~`Description`, pathway0, sum) #有多个通路count数求和
# 顶部柱状图绘制##第一种
#柱状图
library(ggplot2)
library(ggrepel)
#install.packages("tidyquant")
library(tidyquant)
x1<-palette_dark()##调用12种颜色
x1 =matrix(x1)[1:10,1]
p1<-ggplot(pathway0,aes(Description,Count))+
  geom_col(aes(fill=Description))+
  theme_bw()+
  scale_fill_manual(values = matrix(x1)[,1])+
  ggtitle("palette_dark()")+
  labs(x="",y="")+
  theme(legend.position = "none")
p1
# 顶部柱状图绘制##画圆角柱状图##第二种
#柱状图
library(ggplot2)
library(ggchicklet)
p1 <- ggplot(data=pathway0, mapping=aes(x=Description, y=abs(Count))) +
     geom_chicklet(aes(fill=pathway0$Description))+
	   scale_fill_manual(name="",
                    values =colors()[c(26,32,37,52,57,68,73,74,81,82,84,88,100)])+##设置柱状颜色和标签名字
	 geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
	  theme_bw()+ #去除背景色
	 theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid.major = element_line(size=0,colour = "white"),
		axis.title.y = element_text(face="bold",color="black", size=13),
        axis.text.x = element_blank(),panel.grid=element_blank()
  )+xlab(NULL)+
 labs(title = "Important BP in module genes ",y ="Count")+
 theme(plot.title = element_text(face="bold", color="black",hjust = 0.5,size=20)) +##居中
  theme(legend.text= element_text(face="bold", color="black",size=20),###更改字大小(标签)
        # axis.text.x = element_text(size=15),
        axis.text.y = element_text(face="bold", color="black",size=15),
        axis.title.x = element_text(face="bold", color="black",size=15),
        axis.title.y = element_text(face="bold", color="black",size=15))
p1
ggsave(p1, filename="BP_TIAO.pdf", width=12, height=6, units=c("cm"))
