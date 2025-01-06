# F4C
# 绘制指定共同基因boxplot
library(ggpubr)
# 共同基因
ggboxplot(expr, x = "Symbol", y = "Expression",
          title = "Hub_gene with Expression in Normal VS Tumor", ylab = "Expression",
          color = "Group", palette = "jco")##rotate = TRUE#翻转




# F4E
## 总免疫的生存
ggsurvplot(survfit(Surv(time, event)~Immune_infiltration, data=phe), conf.int=F, pval=TRUE)+  
    labs(title = "Survival Analysis of Immune infiltration", x = "Time/Months", y = "Survival probability")
