# 加载必要的包
library(dplyr)
library(tidyr)

rm(list=ls()) 
setwd("D:\\project_data\\南海冬夏季节性和节律性变化1\\审稿回复\\20241212\\其他补充地一些分析\\基因存在于哪个MAG中\\pick_gene") 


# 读取数据
data <- read.table("bin_Phylum_KO.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 方法 1：使用 table() 创建交叉表
cross_table <- table(data$KO, data$Phylum)

# 方法 2：使用 dplyr 和 pivot_wider()（可选）
# cross_table <- data %>%
#   group_by(KO, Phylum) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = Phylum, values_from = count, values_fill = 0)

# 如果需要矩阵形式（仅适用于方法 1）
cross_matrix <- as.matrix(cross_table)

# 查看结果
print(cross_matrix)

# 保存结果
write.table(cross_matrix, "ko_phylum_counts.txt", sep = "\t", quote = FALSE)