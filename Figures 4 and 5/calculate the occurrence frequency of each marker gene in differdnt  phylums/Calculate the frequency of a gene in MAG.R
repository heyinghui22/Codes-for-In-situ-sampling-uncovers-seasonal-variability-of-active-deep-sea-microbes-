# 加载必要的包
library(dplyr)
library(tidyr)

rm(list=ls()) 
setwd("D:\\project_data\\南海冬夏季节性和节律性变化1\\审稿回复\\20241212\\其他补充地一些分析\\基因存在于哪个MAG中\\pick_gene") 


# 读取数据
data <- read.table("bin_Phylum_KO.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 方法 1：使用 table() 创建交叉表
cross_table <- table(data$KO, data$Phylum)

# 方法 2：使用 dplyr 和 pivot_wider()
# cross_table <- data %>%
#   group_by(KO, Phylum) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = Phylum, values_from = count, values_fill = 0)

# 如果需要占比（按行归一化）
cross_matrix <- as.matrix(cross_table)
row_sums <- rowSums(cross_matrix)
prop_matrix <- sweep(cross_matrix, 1, row_sums, "/")
prop_matrix[is.nan(prop_matrix)] <- 0

# 查看结果
print(prop_matrix)

# 保存结果
write.table(prop_matrix, "ko_phylum_matrix.txt", sep = "\t", quote = FALSE)