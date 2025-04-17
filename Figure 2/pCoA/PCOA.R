#getwd()
#setwd("E:\\R\\18S")

#基于Unweighted/weighted UniFrac距离绘制PCoA
#library(GUniFrac) #用于计算Unifrac距离
library(ape) # 用于pcoa分析
library(ggplot2) #用于画图
library(vegan)

#library(ggbiplot)
library(ggrepel)
library(RColorBrewer)

##读文???
setwd("F:")
otu<- read.table( "feature-table.txt",header=TRUE,sep="\t", row.names=1, check.names=FALSE )
#View(otu)

#排序（基??? OTU 丰度表）
distance <- vegdist(t(otu), method = 'bray')

PCOA <- pcoa(distance, correction="none", rn=NULL) #利用PCOA()指令做pcoa分析 归一???
result <-PCOA$values[,"Relative_eig"]
pro1 = as.numeric(sprintf("%.3f",result[1]))*100
pro2 = as.numeric(sprintf("%.3f",result[2]))*100
x = PCOA$vectors
sample_names = rownames(x)
pc = as.data.frame(PCOA$vectors[,1:2])
pc$names = sample_names
legend_title = ""

#group info
design<-read.table( "group.txt",header=TRUE,sep="\t", row.names=1, check.names=FALSE )


#将分组文件和数据文件以行名合???
pc=merge(pc,design,by="row.names",all.x=TRUE)



xlab=paste("PCoA1 (",pro1,"%)",sep="") 
ylab=paste("PCoA2 (",pro2,"%)",sep="")


pca=ggplot(pc,aes(Axis.1,Axis.2)) + #用ggplot作图
  geom_point(aes(color=group)) + 
  #geom_text(aes(label=Row.names),size=4,vjust=-1) +
  #theme(plot.title = element_text(size = 30))+
  #geom_text_repel(aes(Axis.1,Axis.2, label=names))+
  labs(x=xlab,y=ylab,title="18S PCoA") + 
  geom_hline(yintercept=0,linetype=4,color="grey") + 
  geom_vline(xintercept=0,linetype=4,color="grey")

pdf("18sPCOA.pdf")
pca
dev.off()
