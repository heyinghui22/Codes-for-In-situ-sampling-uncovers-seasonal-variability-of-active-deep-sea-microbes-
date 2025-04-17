rm(list=ls()) 
setwd("D:\\project_data\\南海冬夏季节性和节律性变化1\\审稿回复\\20241212\\其他补充地一些分析\\基因存在于哪个MAG中\\pick_gene代码在这里") 

data<- read.table("ko_phylum_counts.txt",head = T, sep = '\t') 
##是基因相对丰度表。第一行是不同的phylum，第一列是所有KO。注意：左上角第一个单元格写“KO” 
df<- read.table("KO号.csv",head = T) 

p1 <- match(df$KO, data$KO) 
p1 
data2 <- data[p1,] 
write.csv(data2, "extracted.csv")

