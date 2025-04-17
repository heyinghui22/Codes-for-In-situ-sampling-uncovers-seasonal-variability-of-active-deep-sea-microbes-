rm(list=ls())#clear Global Environment
setwd("C:\\Users\\user22\\Desktop\\南海冬夏季节性变化\\审稿回复\\20241212\\其他补充地一些分析\\基因在基因组中和转录组中的箱式图")
library(ggplot2)
library(ggpubr)
library(reshape2)

data1<-read.csv("1.csv", header = T)
data_melt <- melt(data1,id.vars = "season")
# 注释：将原始的宽数据变成长数据，方便画图
colnames(data_melt) = c("season","gene","TPM")

#compare_means(TPM~season, data=data_melt)

compaired <- list(c("summer", "winter"))

p1 <- ggplot(data_melt, aes(x = gene, y = TPM))+
  geom_boxplot(outlier.size = 1, aes(fill=factor(season)),
               position = position_dodge(0.8),size=0.5) +  
  guides(fill=guide_legend(title="season"))+
  theme_minimal()+
  theme(axis.title=element_text(size=13,face="plain",color="black"),
        axis.text = element_text(size=11,face="plain",color="black"),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black")) #+ coord_flip()
  #geom_jitter(mapping=aes(x=gene,y=TPM,colour = season), #散点
              #alpha = 0.3,size=3)+

p1

ggsave('cbb.pdf', p1, width =4, height = 4)