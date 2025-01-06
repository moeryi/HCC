# F1E
##模块上下调及热图
##查看模块基因的差异状况
# Module_circular_up，上调
Module_Gene <- read.table("Module_colour_Gene.txt", sep = "\t",stringsAsFactors = F, header = T, fill=TRUE, quote = "")
DEG <- read.table("1.DEG\\DegData_DESeq2.txt",sep = "\t",comment.char = "!", stringsAsFactors = F,head = T, fill=TRUE, quote="")
DEG1 <- DEG[DEG$log2FoldChange > 0,] 
##开始画图
DEG1 =DEG1[,c(1,3,6)]
colnames(DEG1) =c("DEG","logFC","P.Value")
Module_Gene <- merge(Module_Gene,DEG1, by.x = "Symbol",by.y = "DEG",all=FALSE)
#对模块排序
Module_Gene = data.frame(Module_Gene[order(Module_Gene[,2]),])
table(Module_Gene$Module)
Module_Gene$Module =factor(Module_Gene$Module,levels=c("m1","m2","m3","m4","m5","m6","m7","m8"))
##设置颜色
# colour <- ifelse(Module_Gene$logFC > 0,'red','blue')
Module_Gene$colour <- as.character(ifelse(Module_Gene$P.Value < 0.05 & abs(Module_Gene$logFC) >=0,ifelse(Module_Gene$logFC > 0 ,'red','blue'),'grey'))
Module_Gene$P.Value <- -log10(Module_Gene$P.Value)
# Upload library
library(circlize)
circos.par("track.height" = 0.4)##设定扇形高度
# Create data
data =Module_Gene[,c(3,4,5,2)]
colnames(data)<-c("factor","x","y","colour")
# Step1: Initialise the chart giving factor and x-axis.步骤1：初始化图表给出因子和X轴。
circos.initialize( factors=data$factor, x=data$x )

# Step 2: Build the regions.第二步：建立区域。
circos.trackPlotRegion(factors = data$factor, y = data$y, panel.fun = function(x, y) {
    circos.axis()
    })
# Step 3: Add points添加点
circos.trackPoints(data$factor, data$x, data$y, col=data$colour)

# Module_circular_down，下调
Module_Gene <- read.table("Module_colour_Gene.txt", sep = "\t",stringsAsFactors = F, header = T, fill=TRUE, quote = "")
DEG <- read.table("DegData_DESeq2.txt",sep = "\t",comment.char = "!", stringsAsFactors = F,head = T, fill=TRUE, quote="")
DEG1 <- DEG[DEG$log2FoldChange < 0,] 
##开始画图
DEG1 =DEG1[,c(1,3,6)]
colnames(DEG1) =c("DEG","logFC","P.Value")
Module_Gene <- merge(Module_Gene,DEG1, by.x = "Symbol",by.y = "DEG",all=FALSE)
#对模块排序
Module_Gene = data.frame(Module_Gene[order(Module_Gene[,2]),])
table(Module_Gene$Module)
Module_Gene$Module =factor(Module_Gene$Module,levels=c("m2","m3","m4","m5","m6","m7","m8"))
##设置颜色
# colour <- ifelse(Module_Gene$logFC > 0,'red','blue')
Module_Gene$colour <- as.character(ifelse(Module_Gene$P.Value < 0.05 & abs(Module_Gene$logFC) >=0,ifelse(Module_Gene$logFC > 0 ,'red','blue'),'grey'))
Module_Gene$P.Value <- -log10(Module_Gene$P.Value)
# Upload library
library(circlize)
circos.par("track.height" = 0.4)##设定扇形高度
# Create data
data =Module_Gene[,c(3,4,5,2)]
colnames(data)<-c("factor","x","y","colour")
# Step1: Initialise the chart giving factor and x-axis.步骤1：初始化图表给出因子和X轴。
circos.initialize( factors=data$factor, x=data$x )
# Step 2: Build the regions.第二步：建立区域。
circos.trackPlotRegion(factors = data$factor, y = data$y, panel.fun = function(x, y) {
    circos.axis()
    })
# Step 3: Add points添加点
circos.trackPoints(data$factor, data$x, data$y, col=data$colour)
