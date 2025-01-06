# F1B
# 开始画图
##另一种画法
library(ggVennDiagram)
library(ggplot2)	
x <- list(TCGA_up=TCGA_up$DEG,GSE14520_up=GSE14520_up$DEG,GSE25097_up=GSE25097_up$DEG,GSE76427_up =GSE76427_up$DEG)
ggVennDiagram(x) + scale_fill_gradient(low="white",high = "Red")	

##另一种画法
library(ggVennDiagram)
library(ggplot2)	
x <- list(TCGA_down=TCGA_down$DEG,GSE14520_down=GSE14520_down$DEG,GSE25097_down=GSE25097_down$DEG,GSE76427_down=GSE76427_down$DEG)
ggVennDiagram(x) + scale_fill_gradient(low="white",high = "Blue")	

