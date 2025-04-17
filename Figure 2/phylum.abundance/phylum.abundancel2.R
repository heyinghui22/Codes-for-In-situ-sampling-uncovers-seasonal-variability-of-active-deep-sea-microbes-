setwd("D:\\project_data\\MISNAC\\2_16S_18S\\community-usingzpfcode")
getwd()
rm(list = ls())
library(reshape2)
library(ggplot2)

##堆叠双向柱状图
#读取数据
phylum_top10 <- read.csv('qiime2-18-stucure.l4.txt', row.names = 1,sep = "\t" ,stringsAsFactors = FALSE, check.names = FALSE)

#整理成 ggplot2 作图格式
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')
#View(phylum_top10)
#添加分组
group <- read.delim('phylum.abundance.group.txt', sep = '\t', stringsAsFactors = FALSE)
#View(group)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')

#将 Control 转为负值
phylum_top10[which(phylum_top10$group1 == 'MG'), 'value'] <- phylum_top10[which(phylum_top10$group1 == 'MG'), 'value'] * -1

phylum_top10$group2 <- factor(phylum_top10$group2, levels=c("SM4-2","SM3-2","SM2-2","SM4-1","SM3-1","SM2-1","WM6-2","WM5-2","WM4-2","WM3-2","WM2-2","WM1-2","WM6-1","WM5-1","WM4-1","WM3-1","WM2-1","WM1-1"), ordered=TRUE)

p2 <- ggplot(phylum_top10, aes(group2, value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.5) +
  scale_fill_manual(values =  rev(c('#548b54','#BEBADA', '#FFb6c1', '#80B1D3', 
                                             '#20b2aa', '#FB8072', '#FDB462', '#BC80BD','#CCEBC5','#b0b2ff','#ffa07a',
                                             '#B3DE69','#ff82ab','#8DD3C7','#ffbbff','#b0e0e6','#87ceff',"grey"))) +
                                               #scale_fill_manual(values =  rev(c('red','pink','yellow','purple','#8DD3C7', '#FFFFB3',  '#BEBADA', 'blue', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', 'gray',"black","white","orange"))) +
  labs(x = '', y = 'MG         Relative Abundance(%)          MT') +
  geom_hline(yintercept = 0, size = 0.3) +
  #scale_y_reverse 或者scale_x_reverse来逆转坐标轴
  #置换x、y轴时有时候会导致某一坐标轴的坐标被逆转。比如说原x轴的数据是从左往右阅读的，在对换后，原来数据会从下往上给呈现。有时这是个问题。如果x是因子，我们可以对其用带参数limits=rev(levels(…))的函数scale_x_discrete()进行修正。
  #scale_x_discrete(limits =c("W-T1-1" ,"W-T2-1","W-T3-1","W-T4-1","W-T5-1","W-T6-1","W-T2-2","W-T3-2","W-T4-2","W-T5-2","W-T6-2","W-T1-2","WN-1","WN-2","S-T2-1" ,"S-T3-1","S-T4-1","S-T2-2","S-T3-2","S-T4-2") ) +
  #scale_y_continuous(breaks = seq(-100, 100, 25), labels = as.character(abs(seq(-100, 100, 25)))) +
  #scale_x_continuous(breaks = 1:6, labels = as.character(1:6)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 8)) +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), legend.title = element_blank(), legend.text = element_text(size = 8)) +
  coord_flip()

p2

#ggsave('phylum.abundance.l2.pdf', p2, width = 9, height = 5.5)
p4<-p2+theme(legend.position="right")
ggsave('18s.phylum.abundance.l4.right.pdf', p4, width = 12, height = 5)

p3<-p2+guides(fill=FALSE)  #无图例
ggsave('18s.phylum.abundance.l3.noguide.pdf', p3, width = 5, height = 5)
##########################################################################

rm(list = ls())
phylum_top10 <- read.csv('qiime2-18-stucure.l4.txt', row.names = 1,sep = "\t" ,stringsAsFactors = FALSE, check.names = FALSE)

#整理成 ggplot2 作图格式
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')
#View(phylum_top10)
#添加分组
group <- read.delim('phylum.abundance.group.txt', sep = '\t', stringsAsFactors = FALSE)
#View(group)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')

#将 Control 转为负值
phylum_top10[which(phylum_top10$group1 == 'MG'), 'value'] <- phylum_top10[which(phylum_top10$group1 == 'MG'), 'value'] * -1

phylum_top10$group2 <- factor(phylum_top10$group2, levels=c("SM4-2","SM3-2","SM2-2","SM4-1","SM3-1","SM2-1","WNM2","WNM1","WI-N","WI-F","WM6-2","WM5-2","WM4-2","WM3-2","WM2-2","WM1-2","WM6-1","WM5-1","WM4-1","WM3-1","WM2-1","WM1-1"), ordered=TRUE)



#ggplot2 作图
p2 <- ggplot(phylum_top10, aes(group2, value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.5) +
  scale_fill_manual(values =  rev(c('#548b54','#BEBADA', '#FFb6c1', '#80B1D3', 
  '#20b2aa', '#FB8072', '#FDB462', '#BC80BD','#CCEBC5','#b0b2ff','#ffa07a',
  '#B3DE69','#ff82ab','#8DD3C7','#ffbbff','#b0e0e6','#87ceff',"grey"))) +
  #scale_fill_manual(values =  rev(c('red','pink','yellow','purple','#8DD3C7', '#FFFFB3',  '#BEBADA', 'blue', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', 'gray',"black","white","orange"))) +
  labs(x = '', y = 'MG         Relative Abundance(%)          MT') +
  geom_hline(yintercept = 0, size = 0.3) +
  #scale_y_reverse 或者scale_x_reverse来逆转坐标轴
  #置换x、y轴时有时候会导致某一坐标轴的坐标被逆转。比如说原x轴的数据是从左往右阅读的，在对换后，原来数据会从下往上给呈现。有时这是个问题。如果x是因子，我们可以对其用带参数limits=rev(levels(…))的函数scale_x_discrete()进行修正。
  #scale_x_discrete(limits =c("W-T1-1" ,"W-T2-1","W-T3-1","W-T4-1","W-T5-1","W-T6-1","W-T2-2","W-T3-2","W-T4-2","W-T5-2","W-T6-2","W-T1-2","WN-1","WN-2","S-T2-1" ,"S-T3-1","S-T4-1","S-T2-2","S-T3-2","S-T4-2") ) +
  #scale_y_continuous(breaks = seq(-100, 100, 25), labels = as.character(abs(seq(-100, 100, 25)))) +
  #scale_x_continuous(breaks = 1:6, labels = as.character(1:6)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 8)) +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), legend.title = element_blank(), legend.text = element_text(size = 8)) +
  coord_flip()

p2

#ggsave('phylum.abundance.l3.pdf', p2, width = 9, height = 5.5)
p4<-p2+theme(legend.position="right")
ggsave('18s.phylum.abundance.l4.right.pdf', p4, width = 12, height = 5)

p3<-p2+guides(fill="none")  #无图例
ggsave('18s.phylum.abundance.l4.noguide.pdf', p3, width = 5, height = 5)




