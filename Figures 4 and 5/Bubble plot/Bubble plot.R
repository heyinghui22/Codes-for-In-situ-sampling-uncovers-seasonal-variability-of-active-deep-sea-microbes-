setwd("C:\\Users\\user22\\Desktop\\南海冬夏季节性变化\\审稿回复\\20241212\\其他补充地一些分析\\气泡图")

data <- read.csv("gene_mag.csv", header = T)

# 将第二列及之后的列转换为整数
data[, 2:ncol(data)] <- lapply(data[, 2:ncol(data)], as.integer)

library(reshape2)

# 假设data是已经存在的数据框
long_data <- melt(data, id.vars = "Gene")  # id.vars指定不转换的列

data1 <- read.csv("phylum_counts.csv", header = T)

library(dplyr)

# 假设long_data和data1是已经存在的数据框
# long_data包含variable列
# data1包含Phylum列
# 并且variable和Phylum列的内容相同

# 使用inner_join进行内连接合并
merged_data <- inner_join(long_data, data1, by = c("variable" = "Phylum"))

# 使用mutate添加新列，并将新列命名为percent
merged_data <- merged_data %>%
  mutate(percent = .[[3]] / .[[4]])



library(ggplot2)
library(dplyr)

# 假设merged_data是已经存在的数据框，并且包含Gene, variable, value, percent这四列
# 确保percent列是数值型
merged_data$percent <- as.numeric(merged_data$percent)

# 保存 merged_data 数据框为 CSV 文件
# write.csv(merged_data, "merged_data.csv", row.names = FALSE)
#############################################################
# 筛选出 value 不为 0 的数据
filtered_data <- merged_data[merged_data$value != 0, ]

# 将 percent 列分为四个区间
filtered_data$fill_color <- cut(filtered_data$percent,
                                breaks = c(-Inf, 0.25, 0.5, 0.75, Inf),
                                labels = c("#ebd4cb", "#da9f93", "#b6465f", "#890620"))

# 绘制气泡图
bubble_plot <- ggplot(filtered_data, aes(x = variable, y = Gene, size = value, fill = fill_color)) +
  geom_point(alpha = 0.7, shape = 21, stroke = 0.8) +  # 设置气泡外边框和透明度
  scale_size(range = c(3, 18)) +  # 设置气泡大小范围为 3 至 15 
  scale_fill_identity() +  # 使用填充颜色
  labs(title = "Bubble Plot of Gene vs Variable",
       x = "Variable",
       y = "Gene",
       size = "Value",
       fill = "Percent") +  # 添加标题和轴标签
  theme_minimal()  # 使用极简主题

# ggsave("bubble_plot1.pdf", plot = bubble_plot, width = 10, height = 16)


##########################################################################

# 检查并去重 data$Gene
unique_genes <- unique(data$Gene)

# 确保 filtered_data$Gene 的因子水平根据 unique_genes 设置
filtered_data$Gene <- factor(filtered_data$Gene, levels = unique_genes)

# 绘制气泡图
bubble_plot <- ggplot(filtered_data, aes(x = variable, y = Gene, size = value, fill = fill_color)) +
  geom_point(alpha = 0.7, shape = 21, stroke = 0.8) +  # 设置气泡外边框和透明度
  scale_size(range = c(3, 18)) +  # 设置气泡大小范围为 3 至 18 
  scale_fill_identity() +  # 使用填充颜色
  labs(title = "Bubble Plot of Gene vs Variable",
       x = "Variable",
       y = "Gene",
       size = "Value",
       fill = "Percent") +  # 添加标题和轴标签
  theme_minimal()  # 使用极简主题

# 保存气泡图
ggsave("bubble_plot3.pdf", plot = bubble_plot, width = 10, height = 8)
