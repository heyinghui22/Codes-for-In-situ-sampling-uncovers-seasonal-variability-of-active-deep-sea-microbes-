rm(list=ls())#clear Global Environment
setwd("D:\\project_data\\南海冬夏季节性和节律性变化1\\审稿回复\\20241212\\其他补充地一些分析\\MT与MG汇总\\202501的分析_没有gunc\\20250224用这个\\TPM_RNA")

df1 <- read.table(file="SR1_mtTPM.xls",sep="\t", header=T, quote="")
df2 <- read.table(file="SR2_mtTPM.xls",sep="\t", header=T, quote="")
df3 <- read.table(file="SR3_mtTPM.xls",sep="\t", header=T, quote="")
df4 <- read.table(file="SR4_mtTPM.xls",sep="\t", header=T, quote="")
df5 <- read.table(file="SR5_mtTPM.xls",sep="\t", header=T, quote="")
df6 <- read.table(file="SR6_mtTPM.xls",sep="\t", header=T, quote="")
df7 <- read.table(file="WRNA01_mtTPM.xls",sep="\t", header=T, quote="")
df8 <- read.table(file="WRNA02_mtTPM.xls",sep="\t", header=T, quote="")
df9 <- read.table(file="WRNA03_mtTPM.xls",sep="\t", header=T, quote="")
df10 <- read.table(file="WRNA04_mtTPM.xls",sep="\t", header=T, quote="")
df11 <- read.table(file="WRNA05_mtTPM.xls",sep="\t", header=T, quote="")
df12 <- read.table(file="WRNA06_mtTPM.xls",sep="\t", header=T, quote="")
df13 <- read.table(file="WRNA07_mtTPM.xls",sep="\t", header=T, quote="")
df14 <- read.table(file="WRNA08_mtTPM.xls",sep="\t", header=T, quote="")
df15 <- read.table(file="WRNA09_mtTPM.xls",sep="\t", header=T, quote="")
df16 <- read.table(file="WRNA10_mtTPM.xls",sep="\t", header=T, quote="")
df17 <- read.table(file="WRNA11_mtTPM.xls",sep="\t", header=T, quote="")
df18 <- read.table(file="WRNA12_mtTPM.xls",sep="\t", header=T, quote="")



####将两个数据框联合在一起
y =cbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18) 

write.table(y, file="MAG_TPM.txt", sep="\t", row.names=F, quote=F)
x <- read.table(file="MAG_TPM.txt",sep="\t", header=T, quote="")

## 显示数据集的列名
names(x)
## R语言删除某列
library(dplyr)
## 按列名删除多列
data <- select(x,-c(Contig.1, Contig.2 ,Contig.3, Contig.4, Contig.5,Contig.6, Contig.7,Contig.8, Contig.9, Contig.10,Contig.11, Contig.12 ,Contig.13, Contig.14, Contig.15,Contig.16, Contig.17))                   

colnames(data)<- c("Genome","SR1", "SR2", "SR3", "SR4", "SR5", "SR6","WRNA01", "WRNA02", "WRNA03", "WRNA04", "WRNA05", "WRNA06","WRNA07", "WRNA08", "WRNA09", "WRNA10", "WRNA11", "WRNA12")
#"Genome", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6","WRNA01", "WRNA02", "WRNA03", "WRNA04", "WRNA05", "WRNA06", "WRNA07","WRNA08", "WRNA09","WRNA10","WRNA11","WRNA12"

write.table(data, file="decontamination_TPM.txt", sep="\t", row.names=F, quote=F)


#将两个表格合并（写入KO号）
result <- read.table(file="decontamination_TPM.txt",sep="\t", header=T, quote="")
map <-  read.table("DelBlackLine_all.txt", header=T, sep="\t", quote="")
# 提取前两列

# 提取第一列中包含 * 的行
filtered_map <- map[grepl("\\*", map[,1]), ]

map <- filtered_map[, 2:3]

# 在第一行插入列名
colnames(map) <- c("Genome", "ko.number")

result2 = merge(result, map, by.x="Genome", by.y="Genome", all.x=T)
#result3 <- result2[order(result2$average, decreasing = T), ]
write.table(result2, file="1_decontamination_tpm_keggannotation.txt", sep="\t", row.names=F, quote=F)


##### 合并基因名字 #####
genename <-  read.table("kegg_pathway.txt", header=T, sep="\t", quote="")
result2 <- read.table("1_decontamination_tpm_keggannotation.txt", header=T, sep="\t", quote="")
result3 = merge(result2, genename, by.x="ko.number", by.y="KO", all.x=T)

# 按数值型列排序（Genome为数值列）
# data_sorted <- data[order(-data$Genome), ]

# 通用方法（适用于字符/因子/数值列）
result3_sorted <- result3[order(result3$Genome, decreasing = TRUE), ]

write.table(result3_sorted , file="1_keggannotation_withgenename.txt", sep="\t", row.names=F, quote=F)




df2<-aggregate(cbind(result2$SR1, result2$SR2, result2$SR3, result2$SR4, result2$SR5, result2$SR6,result2$WRNA01, result2$WRNA02, result2$WRNA03, result2$WRNA04, result2$WRNA05, result2$WRNA06,result2$WRNA07, result2$WRNA08, result2$WRNA09, result2$WRNA10, result2$WRNA11, result2$WRNA12), by=list(result2$ko.number), FUN = sum)
colnames(df2)<- c("KO", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6","WRNA01", "WRNA02", "WRNA03", "WRNA04", "WRNA05", "WRNA06","WRNA07", "WRNA08", "WRNA09", "WRNA10", "WRNA11", "WRNA12")
#这个表头的顺序是按照result <- read.table这里边的顺序来定的
write.table(df2, file="2_decontamination_merged_keggTPM.txt", sep="\t", row.names=F, quote=F)

##### 合并基因名字 #####
genename <-  read.table("kegg_pathway.txt", header=T, sep="\t", quote="")
result4 = merge(df2, genename, by.x="KO", by.y="KO", all.x=T)
write.table(result4, file="2_keggannotation_withgenename.txt", sep="\t", row.names=F, quote=F)




# 合并相同基因的行结果

#df1 <- data.frame(t(df1))

rm(list=ls())#clear Global Environment

data<- read.table("2_decontamination_merged_keggTPM.txt",head = T)
##是基因相对丰度表。第一行是不同的phylum，第一列是所有KO。注意：左上角第一个单元格写“KO” 
df<- read.table("pickedgeneKO.csv",head = T) 
##注意：自己需要关注的KO 的list，左上角第一个单元格写“KO” 
p1 <- match(df$KO, data$KO) 
p1 
data2 <- data[p1,] 
write.csv(data2, "3_pickedgene.csv")



##提取关注的KO number
data<- read.table("2_decontamination_merged_keggTPM.txt",head = T) 
##是基因相对丰度表。第一行是不同的phylum，第一列是所有KO。注意：左上角第一个单元格写“KO” 
df<- read.csv("40_marke_namer.csv",head = T) 
##注意：自己需要关注的KO 的list，左上角第一个单元格写“KO” 
p1 <- match(df$KO, data$KO) 
p1 
data2 <- data[p1,] 
write.csv(data2, "3_40_marker.csv")


