# =============================================================
# 04_hypothesis_test.R
# ZJX Corridor — RQ1, RQ2, RQ3 전체
#
# 실행 전제: 03_baseline.R 실행 완료
# 필요 객체: daily_counts, hourly_counts, hourly_summary, dow_order
#
# 수정 내역:
#   - Plot H: UTC 16-17 → UTC 7 (실제 유의한 시간대 반영)
#   - RQ3 clustering 추가 (02_comparison_plots.R에서 통합)
#
# 생성 파일 (03_Baseline_EDA/ 폴더):
#   table_hourly_kruskal.csv
#   table_dunn_utc7.csv
#   plot_G_hourly_pvalue.png
#   plot_H_significant_hour_utc7.png
#   plot_F_dendrogram.png
#   table_cluster_assignment.csv
#   table_distance_correlation.csv
#   table_pairwise_all.csv
# =============================================================

library(tidyverse)
library(rstatix)

plot_dir <- "03_Baseline_EDA"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)


# ============================================================
# RQ1: 요일 간 daily flight count 차이
# H0: 요일 간 분포 동일
# ============================================================
daily_counts_f <- daily_counts %>%
  mutate(day_of_week = factor(as.character(day_of_week),
                              levels = dow_order))

kw_daily <- kruskal.test(n_flights ~ day_of_week,
                         data = daily_counts_f)
cat("=== RQ1: Kruskal-Wallis (daily count by DOW) ===\n")
print(kw_daily)


# ============================================================
# RQ2: 시간대별 Kruskal-Wallis (UTC 7-18 피크 구간)
# H0: 특정 시간대의 요일 간 count 분포 동일
# ============================================================
hourly_counts_f <- hourly_counts %>%
  mutate(day_of_week = factor(as.character(day_of_week),
                              levels = dow_order))

hourly_kw <- hourly_counts_f %>%
  group_by(hour) %>%
  summarise(
    kw_stat = kruskal.test(n_flights ~ day_of_week)$statistic,
    kw_pval = kruskal.test(n_flights ~ day_of_week)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    significant = kw_pval < 0.05,
    pval_label  = round(kw_pval, 3)
  )

cat("\n=== RQ2: 시간대별 Kruskal-Wallis (UTC 7-18) ===\n")
print(hourly_kw)
write_csv(hourly_kw, file.path(plot_dir, "table_hourly_kruskal.csv"))

cat("\n유의한 시간대 (p < 0.05):\n")
print(hourly_kw %>% filter(significant))


# ── Plot G: 시간대별 p-value bar chart ───────────────────────
pG <- ggplot(hourly_kw, aes(x = hour, y = kw_pval)) +
  geom_col(aes(fill = significant), width = 0.7) +
  geom_hline(yintercept = 0.05, color = "red",
             linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(
    values = c("TRUE" = "#2C7BB6", "FALSE" = "#d3d3d3"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = ""
  ) +
  scale_x_continuous(breaks = 7:18) +
  annotate("text", x = 18.3, y = 0.065,
           label = "p = 0.05", color = "red", size = 3.2) +
  labs(
    title    = "ZJX Corridor: Kruskal-Wallis p-value by Hour of Day",
    subtitle = "Peak operational window UTC 7–18. Red line = p = 0.05. Blue = significant.",
    x = "Hour (UTC)", y = "p-value"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(plot_dir, "plot_G_hourly_pvalue.png"),
       pG, width = 10, height = 5, dpi = 150)
cat("Saved: plot_G_hourly_pvalue.png\n")


# ── Dunn post-hoc: 유의한 시간대 자동으로 돌림 ───────────────
# 현재 결과: UTC 7만 유의 (p = 0.043)
sig_hours <- hourly_kw %>% filter(significant) %>% pull(hour)

for (h in sig_hours) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("UTC", h, ":00 — Dunn Post-hoc (Bonferroni)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  hour_data <- hourly_counts_f %>%
    filter(hour == h)

  cat("\n요일별 기술통계:\n")
  hour_data %>%
    group_by(day_of_week) %>%
    summarise(
      n    = n(),
      mean = round(mean(n_flights), 1),
      sd   = round(sd(n_flights), 1),
      min  = min(n_flights),
      max  = max(n_flights),
      .groups = "drop"
    ) %>% print()

  dunn <- hour_data %>%
    dunn_test(n_flights ~ day_of_week,
              p.adjust.method = "bonferroni")

  cat("\n전체 결과 (p.adj 오름차순):\n")
  print(dunn %>% arrange(p.adj) %>%
          select(group1, group2, statistic, p, p.adj, p.adj.signif))

  cat("\n유의한 쌍 (p.adj < 0.05):\n")
  sig <- dunn %>% filter(p.adj < 0.05)
  if (nrow(sig) > 0) {
    print(sig %>% select(group1, group2, p.adj, p.adj.signif))
  } else {
    cat("  없음\n")
  }

  write_csv(dunn, file.path(plot_dir, paste0("table_dunn_utc", h, ".csv")))
  cat("Saved: table_dunn_utc", h, ".csv\n", sep = "")
}


# ── Plot H: 유의한 시간대 boxplot (UTC 7) ────────────────────
# UTC 16-17 → UTC 7 으로 수정
plot_data_h <- hourly_counts_f %>%
  filter(hour %in% sig_hours) %>%
  mutate(hour_label = paste0("UTC ", hour, ":00"))

pH <- ggplot(plot_data_h,
             aes(x = day_of_week, y = n_flights,
                 fill = day_of_week)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8,
              show.legend = FALSE) +
  facet_wrap(~ hour_label, nrow = 1) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "ZJX Corridor: Flight Count at Significant Hour Slots by Day of Week",
    subtitle = paste0("UTC ", paste(sig_hours, collapse = ", "),
                      " — only hour(s) with significant day-of-week differences (p < 0.05)."),
    x = NULL, y = "Unique Flights"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave(file.path(plot_dir, "plot_H_significant_hour_utc7.png"),
       pH, width = 7, height = 5, dpi = 150)
cat("Saved: plot_H_significant_hour_utc7.png\n")


# ============================================================
# RQ3: Clustering — 요일별 hourly profile 유사도
# (02_comparison_plots.R 통합)
# ============================================================

# ── 1. 거리 행렬 계산 ─────────────────────────────────────────
# 각 요일을 12개 시간대 mean_flights 벡터로 표현
# 상관계수 기반 거리: 패턴 형태 유사도 (절대값 아닌 모양 비교)
dow_matrix <- hourly_summary %>%
  select(day_of_week, hour, mean_flights) %>%
  pivot_wider(names_from = hour, values_from = mean_flights) %>%
  column_to_rownames("day_of_week") %>%
  as.matrix()

# Euclidean distance (절대 수준 차이)
dist_euc <- dist(dow_matrix, method = "euclidean")

# Correlation-based distance (패턴 형태 차이)
cor_mat  <- cor(t(dow_matrix))
dist_cor <- as.dist(1 - cor_mat)

cat("\n=== 상관 기반 거리 행렬 (낮을수록 패턴 유사) ===\n")
print(round(as.matrix(dist_cor), 3))

write.csv(round(as.matrix(dist_euc), 1),
          file.path(plot_dir, "table_distance_euclidean.csv"))
write.csv(round(as.matrix(dist_cor), 3),
          file.path(plot_dir, "table_distance_correlation.csv"))


# ── 2. 계층적 군집화 + Dendrogram ────────────────────────────
hc <- hclust(dist_cor, method = "ward.D2")
clusters <- cutree(hc, k = 3)

cat("\n=== 3개 클러스터 할당 결과 ===\n")
print(clusters)

cluster_df <- data.frame(
  day_of_week = names(clusters),
  cluster     = as.integer(clusters)
)
write.csv(cluster_df,
          file.path(plot_dir, "table_cluster_assignment.csv"),
          row.names = FALSE)

# Plot F: Dendrogram
png(file.path(plot_dir, "plot_F_dendrogram.png"),
    width = 900, height = 600, res = 150)
par(mar = c(4, 4, 3, 1))
plot(hc,
     main = "ZJX Corridor: Day-of-Week Clustering by Hourly Profile",
     sub  = "Distance = 1 - Pearson correlation of mean hourly flight counts",
     xlab = "", ylab = "Distance (1 - r)", hang = -1)
rect.hclust(hc, k = 3, border = c("#2C7BB6", "#D7191C", "#1A9641"))
dev.off()
cat("Saved: plot_F_dendrogram.png\n")


################# K = 4
# ── 2. 계층적 군집화 + Dendrogram ────────────────────────────
hc <- hclust(dist_cor, method = "ward.D2")

k_cluster <- 4
clusters <- cutree(hc, k = k_cluster)

cat("\n===", k_cluster, "개 클러스터 할당 결과 ===\n")
print(clusters)

cluster_df <- data.frame(
  day_of_week = names(clusters),
  cluster     = as.integer(clusters)
)

write.csv(
  cluster_df,
  file.path(plot_dir, paste0("table_cluster_assignment_k", k_cluster, ".csv")),
  row.names = FALSE
)

# Plot F: Dendrogram
png(
  file.path(plot_dir, paste0("plot_F_dendrogram_k", k_cluster, ".png"))
)

par(mar = c(4, 4, 3, 1))

plot(
  hc,
  main = "Hourly Traffic Pattern - Dendrogram K=4",
  #sub  = "",
  xlab = "", ylab = "Distance (1 - r)", hang = -1)

rect.hclust(
  hc,
  k = k_cluster,
  border = c("#2C7BB6", "#D7191C", "#1A9641", "#984EA3")
)

dev.off()

cat("Saved:", paste0("plot_F_dendrogram_k", k_cluster, ".png"), "\n")

############## K = 5

k_cluster <- 5
clusters <- cutree(hc, k = k_cluster)

cluster_df <- data.frame(
  day_of_week = names(clusters),
  cluster = as.integer(clusters)
)

write.csv(
  cluster_df,
  file.path(plot_dir, paste0("table_cluster_assignment_k", k_cluster, ".csv")),
  row.names = FALSE
)

png(
  file.path(plot_dir, paste0("plot_F_dendrogram_k", k_cluster, ".png")),
  width = 900,
  height = 600,
  res = 150
)

par(mar = c(4, 4, 3, 1))

plot(
  hc,
  main = "ZJX Corridor: Day-of-Week Clustering by Hourly Profile",
  sub  = "Distance = 1 - Pearson correlation of mean hourly flight counts",
  xlab = "",
  ylab = "Distance (1 - r)",
  hang = -1
)

rect.hclust(
  hc,
  k = k_cluster,
  border = c("#2C7BB6", "#D7191C", "#1A9643", "#984EA3", "#FF7F00")
)

dev.off()

# ── 3. Pairwise 비교 테이블 ───────────────────────────────────
# 모든 요일쌍 × 시간대 조합의 mean 차이와 effect size
day_pairs    <- combn(dow_order, 2, simplify = FALSE)
pairwise_rows <- list()

for (pair in day_pairs) {
  d1 <- pair[1]; d2 <- pair[2]
  df1 <- hourly_summary %>%
    filter(day_of_week == d1) %>%
    select(hour, mean1 = mean_flights, sd1 = sd_flights)
  df2 <- hourly_summary %>%
    filter(day_of_week == d2) %>%
    select(hour, mean2 = mean_flights, sd2 = sd_flights)

  joined <- inner_join(df1, df2, by = "hour") %>%
    mutate(
      day1        = d1,
      day2        = d2,
      diff        = round(mean1 - mean2, 1),
      abs_diff    = round(abs(mean1 - mean2), 1),
      pooled_sd   = round(sqrt((sd1^2 + sd2^2) / 2), 2),
      effect_size = round(abs(mean1 - mean2) / pooled_sd, 2)
    ) %>%
    select(day1, day2, hour, mean1, mean2, diff, abs_diff, effect_size)

  pairwise_rows[[length(pairwise_rows) + 1]] <- joined
}

pairwise_df <- bind_rows(pairwise_rows)
write_csv(pairwise_df,
          file.path(plot_dir, "table_pairwise_all.csv"))
cat("Saved: table_pairwise_all.csv (", nrow(pairwise_df), "rows)\n")

# UTC 7시 기준 요일쌍 차이 상위 10
cat("\n=== UTC 7시 기준 요일쌍 차이 TOP 10 ===\n")
pairwise_df %>%
  filter(hour == 7) %>%
  arrange(desc(abs_diff)) %>%
  head(10) %>%
  print()


# ============================================================
# 논문 결과 요약
# ============================================================
cat("\n", paste(rep("=", 55), collapse = ""), "\n")
cat("논문 Results 섹션 핵심 수치\n")
cat(paste(rep("=", 55), collapse = ""), "\n")

cat("RQ1 — Kruskal-Wallis (daily count by DOW):\n")
cat("  H =", round(kw_daily$statistic, 2),
    " df =", kw_daily$parameter,
    " p =", round(kw_daily$p.value, 3), "\n")
cat("  결론: 요일 간 daily count 차이 없음 → baseline 안정성 확인\n\n")

cat("RQ2 — 유의한 시간대 (peak window UTC 7-18):\n")
hourly_kw %>%
  filter(significant) %>%
  mutate(kw_stat = round(kw_stat, 2),
         kw_pval = round(kw_pval, 4)) %>%
  select(hour, kw_stat, kw_pval) %>%
  print()
cat("  결론: UTC 7시에서만 요일 차이 유의 → Mon vs Sun (p.adj = 0.039)\n\n")

cat("RQ3 — Clustering 결과:\n")
for (k in 1:3) {
  days_in_cluster <- names(clusters[clusters == k])
  cat("  Cluster", k, ":", paste(days_in_cluster, collapse = ", "), "\n")
}

cat("\n=== 04_hypothesis_test.R 완료 ===\n")
