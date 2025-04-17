rm(list=ls()) 
setwd("D:\\project_data\\南海冬夏季节性和节律性变化1\\审稿回复\\20241212\\其他补充地一些分析\\基因存在于哪个MAG中\\pick_gene") 

# 加载 dplyr（可选）
library(dplyr)

# 读取文件
gtdbtk <- read.table("GTDBTK.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
delblackline <- read.table("DelBlackLine_all.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 检查文件
head(gtdbtk)
head(delblackline)

# 合并文件（使用 merge 或 dplyr）
# 方法 1：使用 merge
merged_data <- merge(gtdbtk, delblackline, by = "bin", all = FALSE)

# 方法 2：使用 dplyr
# merged_data <- inner_join(gtdbtk, delblackline, by = "ID")

# 检查结果
head(merged_data)
dim(merged_data)

# 保存结果（可选）
write.table(merged_data, "bin_Phylum_KO.txt", sep = "\t", row.names = FALSE, quote = FALSE)