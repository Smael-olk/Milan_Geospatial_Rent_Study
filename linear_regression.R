# ================================================================
# 胡娇的线性回归 — 完整最终版
# 使用方法：Ctrl+A 全选 → Ctrl+Enter 运行
# 需要文件：treated_surface.csv（放在milan project2文件夹里）
# ================================================================


# ── 第1步：加载包 ─────────────────────────────────
library(ggplot2)


# ── 第2步：读取数据 ───────────────────────────────
df <- read.csv("treated_surface.csv")
cat("读取完成，总行数:", nrow(df), "\n")


# ── 第3步：数据清理 ───────────────────────────────
df <- df[!is.na(df$price_n)  & df$price_n  > 100,       ]
df <- df[!is.na(df$price_sqm)& df$price_sqm > 5
         & df$price_sqm < 150,     ]
df <- df[!is.na(df$latitude) & !is.na(df$longitude),    ]
df$rooms_n[is.na(df$rooms_n)]         <- median(df$rooms_n,    na.rm=TRUE)
df$bathrooms[is.na(df$bathrooms)]     <- 1
cat("清理后行数:", nrow(df), "\n")


# ── 第4步：构造地理特征 ───────────────────────────
# 大区均价（粗粒度地理位置）
zone_avg          <- tapply(df$price_n, df$macrozone, mean, na.rm=TRUE)
df$zone_avg_price <- zone_avg[df$macrozone]

# 小区均价（细粒度地理位置，比大区更准）
micro_avg          <- tapply(df$price_n, df$microzone, mean, na.rm=TRUE)
df$micro_avg_price <- micro_avg[df$microzone]

# 供暖类型（转成0/1）
df$heating_auto    <- as.integer(df$heating == "Autonomo")
df$heating_central <- as.integer(df$heating == "Centralizzato")


# ── 第5步：选择特征 ───────────────────────────────
feature_cols <- c(
  "surface_room",    # 单间实际面积（Mathilde修正过的）
  "rooms_n",         # 房间数
  "bathrooms",       # 浴室数
  "latitude",        # 纬度
  "longitude",       # 经度
  "zone_avg_price",  # 大区均价
  "micro_avg_price", # 小区均价（更精细的地理位置）
  "heating_auto",    # 独立供暖
  "heating_central"  # 集中供暖
)

df_model <- df[, c(feature_cols, "price_n")]
df_model <- df_model[complete.cases(df_model), ]
cat("建模数据:", nrow(df_model), "套，", length(feature_cols), "个特征\n")


# ── 第6步：切分训练集/测试集 ─────────────────────
set.seed(42)
idx   <- sample(1:nrow(df_model), size = round(0.8 * nrow(df_model)))
train <- df_model[idx,  ]
test  <- df_model[-idx, ]
cat("训练集:", nrow(train), "套 | 测试集:", nrow(test), "套\n")


# ── 第7步：跑OLS线性回归 ─────────────────────────
ols  <- lm(price_n ~ ., data = train)

# 统计报告（截图发给Ismail）
cat("\n=== OLS统计报告 ===\n")
print(summary(ols))


# ── 第8步：评估模型 ───────────────────────────────
pred <- predict(ols, newdata = test)
r2   <- cor(test$price_n, pred)^2
rmse <- sqrt(mean((test$price_n - pred)^2))
mae  <- mean(abs(test$price_n - pred))

cat("\n========================================\n")
cat("OLS线性回归最终结果（截图发给组里）\n")
cat("========================================\n")
cat(sprintf("R²   = %.3f  （解释了%.1f%%的价格变化）\n", r2, r2*100))
cat(sprintf("RMSE = %.0f 欧 （平均预测误差）\n", rmse))
cat(sprintf("MAE  = %.0f 欧\n", mae))
cat(sprintf("样本 = %d 套租房\n", nrow(df_model)))
cat("========================================\n")


# ── 第9步：画图并保存 ────────────────────────────
ggplot(data.frame(真实 = test$price_n, 预测 = pred),
       aes(x = 真实, y = 预测)) +
  geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "OLS线性回归：预测 vs 真实价格",
    subtitle = sprintf("R²=%.3f，RMSE=%.0f欧/月 | 点越靠近红线=预测越准",
                       r2, rmse),
    x = "真实价格（欧元）",
    y = "OLS预测价格（欧元）"
  ) +
  theme_minimal(base_size = 11)

ggsave("OLS线性回归结果.png", width = 7, height = 6, dpi = 150)
cat("图片已保存：OLS线性回归结果.png\n")
cat("\n全部完成！\n")
library(ggplot2)

# 读取Mathilde的新数据（含交通密度）
df <- read.csv("clean_data_mobility.csv")
cat("列名:\n")
print(names(df))
cat("行数:", nrow(df), "\n")
library(ggplot2)

# 数据清理
df <- df[!is.na(df$price_n) & df$price_n > 100, ]
df <- df[!is.na(df$price_sqm) & df$price_sqm > 5 & df$price_sqm < 150, ]
df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
df$rooms_n[is.na(df$rooms_n)] <- median(df$rooms_n, na.rm=TRUE)
df$bathrooms[is.na(df$bathrooms)] <- 1

# 地理特征
zone_avg <- tapply(df$price_n, df$macrozone, mean, na.rm=TRUE)
df$zone_avg_price <- zone_avg[df$macrozone]
micro_avg <- tapply(df$price_n, df$microzone, mean, na.rm=TRUE)
df$micro_avg_price <- micro_avg[df$microzone]

# 建模特征（加入交通密度+设施）
feature_cols <- c(
  "surface_room", "rooms_n", "bathrooms",
  "latitude", "longitude",
  "zone_avg_price", "micro_avg_price",
  "heating_Autonomo", "heating_Centralizzato",
  "bikemi_density_500m",
  "metro_density_750m",
  "tram_density_500m",
  "balcone", "ascensore", "lavatrice",
  "aria.condizionata", "arredato", "portineria",
  "lavastoviglie", "terrazzo", "giardino"
)

df_model <- df[, c(feature_cols, "price_n")]
df_model <- df_model[complete.cases(df_model), ]
cat("建模数据:", nrow(df_model), "套,", length(feature_cols), "个特征\n")

set.seed(42)
idx   <- sample(1:nrow(df_model), size=round(0.8*nrow(df_model)))
train <- df_model[idx, ]
test  <- df_model[-idx, ]

# 跑OLS
ols  <- lm(price_n ~ ., data=train)
pred <- predict(ols, newdata=test)
r2   <- cor(test$price_n, pred)^2
rmse <- sqrt(mean((test$price_n - pred)^2))
mae  <- mean(abs(test$price_n - pred))

cat("\n========================================\n")
cat("最终OLS结果（加入交通+设施特征）\n")
cat("========================================\n")
cat(sprintf("R²   = %.3f（解释了%.1f%%的价格变化）\n", r2, r2*100))
cat(sprintf("RMSE = %.0f 欧\n", rmse))
cat(sprintf("MAE  = %.0f 欧\n", mae))
cat("========================================\n")

# 画图
ggplot(data.frame(真实=test$price_n, 预测=pred),
       aes(x=真实, y=预测)) +
  geom_point(alpha=0.3, size=0.8, color="steelblue") +
  geom_abline(slope=1, intercept=0, color="red",
              linetype="dashed", linewidth=1) +
  labs(title="OLS线性回归（最终版）：预测 vs 真实价格",
       subtitle=sprintf("R²=%.3f，RMSE=%.0f欧/月", r2, rmse),
       x="真实价格（欧元）", y="OLS预测价格（欧元）") +
  theme_minimal(base_size=11)

ggsave("OLS_最终版.png", width=7, height=6, dpi=150)
cat("图片已保存：OLS_最终版.png\n")
