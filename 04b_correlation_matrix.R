# =============================================================
# 04b_correlation_matrix.R (버그 수정본)
#
# 수정 내역:
#   1. CSV = 거리행렬 → 1 - distance 로 상관행렬 변환
#   2. 중복 쌍 제거: pmin/pmax 알파벳 정렬 방식
#   3. Sunday 필터 버그 수정
# =============================================================

library(tidyverse)

plot_dir <- "03_Baseline_EDA"
dow_order <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

# ── 1. CSV에서 거리행렬 읽기 → 상관행렬 변환 ─────────────────
dist_raw <- read.csv(
  file.path(plot_dir, "table_distance_correlation.csv"),
  row.names = 1, check.names = FALSE
)

dist_mat  <- as.matrix(dist_raw)[dow_order, dow_order]
corr_matrix <- round(1 - dist_mat, 3)
diag(corr_matrix) <- 1.000

cat("=== Correlation Matrix (1 - distance) ===\n")
print(corr_matrix)

# ── 2. Long format ────────────────────────────────────────────
corr_long <- as.data.frame(corr_matrix) %>%
  rownames_to_column("day1") %>%
  pivot_longer(-day1, names_to = "day2", values_to = "correlation") %>%
  mutate(
    day1 = factor(day1, levels = dow_order),
    day2 = factor(day2, levels = rev(dow_order))
  )

# ── 3. Heatmap ────────────────────────────────────────────────
p_corr <- ggplot(corr_long,
                 aes(x = day1, y = day2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%.3f", correlation),
        color  = correlation < 0.85),
    size = 3.8, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#f7fbff", mid = "#6baed6", high = "#08306b",
    midpoint = 0.85, limits = c(0.6, 1.0),
    name = "Pearson r\n(pattern\nsimilarity)"
  ) +
  scale_color_manual(values = c("TRUE" = "gray20", "FALSE" = "white"),
                     guide = "none") +
  scale_x_discrete(position = "top") +
  labs(
    title    = "ZJX Corridor: Day-of-Week Traffic Profile Correlation Matrix",
    subtitle = "r = Pearson correlation of mean hourly flight counts (UTC 7–18). Higher = more similar.",
    x = NULL, y = NULL
  ) +
  coord_fixed() +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.text.x   = element_text(face = "bold", size = 12),
    axis.text.y   = element_text(face = "bold", size = 12),
    legend.position = "right",
    panel.grid    = element_blank()
  )

ggsave(file.path(plot_dir, "plot_RQ3_correlation_matrix.png"),
       p_corr, width = 8, height = 7, dpi = 150)
cat("Saved: plot_RQ3_correlation_matrix.png\n")

# ── 4. 핵심 수치 (버그 수정: pmin/pmax로 중복 제거) ──────────
corr_pairs <- corr_long %>%
  mutate(d1 = as.character(day1),
         d2 = as.character(day2)) %>%
  filter(d1 != d2) %>%
  mutate(pair = paste(pmin(d1, d2), pmax(d1, d2), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  arrange(desc(correlation))

cat("\n=== 가장 유사한 패턴 TOP 5 ===\n")
corr_pairs %>%
  head(5) %>%
  mutate(grade = case_when(
    correlation >= 0.95 ~ "거의 동일",
    correlation >= 0.90 ~ "매우 유사",
    TRUE ~ "유사"
  )) %>%
  select(d1, d2, correlation, grade) %>%
  print()

cat("\n=== 가장 다른 패턴 BOTTOM 5 ===\n")
corr_pairs %>%
  tail(5) %>%
  arrange(correlation) %>%
  mutate(grade = case_when(
    correlation < 0.70 ~ "구조적으로 상이",
    correlation < 0.80 ~ "패턴 차이 있음",
    TRUE ~ "약간 다름"
  )) %>%
  select(d1, d2, correlation, grade) %>%
  print()

cat("\n=== Sunday 전체 상관 (오름차순) ===\n")
corr_pairs %>%
  filter(d1 == "Sun" | d2 == "Sun") %>%
  arrange(correlation) %>%
  select(d1, d2, correlation) %>%
  print()

write_csv(corr_pairs %>% select(d1, d2, correlation),
          file.path(plot_dir, "table_correlation_pairs.csv"))
cat("\nSaved: table_correlation_pairs.csv\n")
