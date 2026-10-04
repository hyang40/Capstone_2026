# =============================================================
# 03_hypothesis_tests.R
# ZJX Corridor — 가설 검정 전체
#
# 실행 전제: 01_kmxr_data_prep.R 실행 완료
# 필요 객체: daily_counts, hourly_counts, dow_order
#
# 핵심 수정 사항 (에러 수정):
#   - FSA::dunnTest() 제거 → rstatix::dunn_test() 사용
#   - day_of_week: factor(as.character(...)) 명시적 변환
#     (ordered factor 잔류 속성 제거)
#   - hour_utc 변수명 → hour 로 통일 (01 파일과 일치)
#
# 생성 파일: table_hourly_kruskal.csv
#            table_dunn_utc16.csv
#            table_dunn_utc17.csv
#            plot_G_hourly_pvalue.png
#            plot_H_significant_hours.png
# =============================================================

library(tidyverse)
library(rstatix)

### After running 03.R

# ── RQ1: 요일 간 daily flight count 차이 ─────────────────────
# 수정: factor(as.character()) 로 명시 변환 — 에러 방지 핵심
daily_counts_f <- daily_counts %>%
  mutate(day_of_week = factor(as.character(day_of_week),
                              levels = dow_order))

kw_daily <- kruskal.test(n_flights ~ day_of_week,
                         data = daily_counts_f)
cat("=== RQ1: Kruskal-Wallis (daily count by DOW) ===\n")
print(kw_daily)

# ── RQ2: 시간대별 Kruskal-Wallis ─────────────────────────────
hourly_counts_f <- hourly_counts %>%
  mutate(day_of_week = factor(as.character(day_of_week),
                              levels = dow_order))

hourly_kw <- hourly_counts_f %>%
  group_by(hour) %>%           # ← 수정: hour_utc → hour
  summarise(
    kw_stat = kruskal.test(n_flights ~ day_of_week)$statistic,
    kw_pval = kruskal.test(n_flights ~ day_of_week)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    significant = kw_pval < 0.05,
    pval_label  = round(kw_pval, 3)
  )

cat("\n=== RQ2: 시간대별 Kruskal-Wallis ===\n")
print(hourly_kw)
write_csv(hourly_kw, "table_hourly_kruskal.csv")

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
  annotate("text", x = 18.4, y = 0.065,
           label = "p = 0.05", color = "red", size = 3.5) +
  labs(
    title    = "ZJX Corridor: Kruskal-Wallis p-value by Hour of Day",
    subtitle = "Red line = significance threshold (p = 0.05). Blue = significant.",
    x = "Hour (UTC)", y = "p-value"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plot_G_hourly_pvalue.png", pG, width = 10, height = 5, dpi = 150)
cat("Saved: plot_G_hourly_pvalue.png\n")

# ── UTC 16, 17시 Dunn post-hoc ────────────────────────────────
# 수정: hour_utc → hour, rstatix::dunn_test() 사용
sig_hours <- hourly_kw %>% filter(significant) %>% pull(hour)

for (h in sig_hours) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("UTC", h, "시 — Dunn Post-hoc (Bonferroni)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  hour_data <- hourly_counts_f %>%
    filter(hour == h)   # ← 수정: hour_utc → hour
  
  # 요일별 기술통계
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
  
  # Dunn test (rstatix — FSA 에러 없음)
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
  
  write_csv(dunn, paste0("table_dunn_utc", h, ".csv"))
  cat("Saved: table_dunn_utc", h, ".csv\n", sep = "")
}

# ── Plot H: UTC 16–17 boxplot ────────────────────────────────
plot_data <- hourly_counts_f %>%
  filter(hour %in% c(16, 17)) %>%    # ← 수정: hour_utc → hour
  mutate(hour_label = paste0("UTC ", hour, ":00"))

pH <- ggplot(plot_data,
             aes(x = day_of_week, y = n_flights,
                 fill = day_of_week)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8,
              show.legend = FALSE) +
  facet_wrap(~ hour_label, nrow = 1) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "ZJX Corridor: Flight Count at UTC 16–17 by Day of Week",
    subtitle = "Only hours with significant day-of-week differences (p < 0.05).",
    x = NULL, y = "Unique Flights"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("plot_H_significant_hours.png", pH,
       width = 10, height = 5, dpi = 150)
cat("Saved: plot_H_significant_hours.png\n")

# ── 추가: 오후 하강 기울기 (접근 4) ──────────────────────────
afternoon_slope <- hourly_counts_f %>%
  filter(hour >= 12) %>%             # ← 수정: hour_utc → hour
  group_by(date, day_of_week) %>%
  summarise(
    slope = coef(lm(n_flights ~ hour))[2],   # ← 수정: hour_utc → hour
    .groups = "drop"
  )

cat("\n=== 요일별 오후 하강 기울기 (음수일수록 급감) ===\n")
afternoon_slope %>%
  group_by(day_of_week) %>%
  summarise(
    mean_slope = round(mean(slope), 2),
    sd_slope   = round(sd(slope), 2),
    .groups = "drop"
  ) %>% print()

kw_slope <- kruskal.test(slope ~ day_of_week, data = afternoon_slope)
cat("\n=== 오후 기울기 Kruskal-Wallis ===\n")
print(kw_slope)

# ── 논문 결과 요약 ─────────────────────────────────────────
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("논문 Results 섹션 핵심 수치\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("RQ1 Kruskal-Wallis: H =", round(kw_daily$statistic, 2),
    " df =", kw_daily$parameter,
    " p =", round(kw_daily$p.value, 3), "\n")
cat("결론: 요일 간 daily count 차이 없음 → baseline 안정성 확인\n\n")
cat("RQ2 유의한 시간대:\n")
hourly_kw %>% filter(significant) %>%
  select(hour, kw_stat, kw_pval) %>%
  mutate(kw_stat = round(kw_stat, 2),
         kw_pval = round(kw_pval, 4)) %>%
  print()
