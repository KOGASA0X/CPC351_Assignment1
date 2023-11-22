# 读取CSV文件
data <- read.csv("tracks_features.csv")

# 计算文件数量
num_files <- 40

# 对于每个文件
for (i in 1:num_files) {
  # 计算列的范围
  start_col <- ((i - 1) %% (ncol(data) / 3)) * 3 + 1
  end_col <- min(start_col + 2, ncol(data))
  
  # 计算行的范围
  start_row <- ((i - 1) %/% (ncol(data) / 3)) * 250000 + 1
  end_row <- min(start_row + 249999, nrow(data))
  
  # 选择相应的行和列
  subset <- data[start_row:end_row, start_col:end_col]
  
  # 写入新的CSV文件
  write.csv(subset, paste0("Q4/spotify_", i, ".csv"), row.names = FALSE)
}