# =============================================================
# 04_hypothesis_test.R
# ZJX Corridor — RQ1, RQ2, RQ3 
# =============================================================

library(tidyverse)
library(rstatix)

plot_dir <- "03_Baseline_EDA"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)


# ============================================================
# RQ1: daily flight count difference
# ============================================================
daily_counts_f <- daily_counts %>%
  mutate(day_of_week = factor(as.character(day_of_week),
                              levels = dow_order))

kw_daily <- kruskal.test(n_flights ~ day_of_week,
                         data = daily_counts_f)
cat("=== RQ1: Kruskal-Wallis (daily count by DOW) ===\n")
print(kw_daily)


# ============================================================
# RQ2: Kruskal-Wallis test - UTC 7-18 
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

print(hourly_kw)
write_csv(hourly_kw, file.path(plot_dir, "table_hourly_kruskal.csv"))

print(hourly_kw %>% filter(significant))


# Plot G: p-value bar chart 
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


# Dunn post-hoc
# UTC 7
sig_hours <- hourly_kw %>% filter(significant) %>% pull(hour)

for (h in sig_hours) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("UTC", h, ":00 — Dunn Post-hoc (Bonferroni)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  hour_data <- hourly_counts_f %>%
    filter(hour == h)

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

  print(dunn %>% arrange(p.adj) %>%
          select(group1, group2, statistic, p, p.adj, p.adj.signif))

  sig <- dunn %>% filter(p.adj < 0.05)
  if (nrow(sig) > 0) {
    print(sig %>% select(group1, group2, p.adj, p.adj.signif))
  } else {
  }

  write_csv(dunn, file.path(plot_dir, paste0("table_dunn_utc", h, ".csv")))
  cat("Saved: table_dunn_utc", h, ".csv\n", sep = "")
}


# Plot H:boxplot (UTC 7)
# UTC 7 
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
# RQ3: Important date Clustering
# ============================================================

#Step 1
dow_matrix <- hourly_summary %>%
  select(day_of_week, hour, mean_flights) %>%
  pivot_wider(names_from = hour, values_from = mean_flights) %>%
  column_to_rownames("day_of_week") %>%
  as.matrix()

# Euclidean distance
dist_euc <- dist(dow_matrix, method = "euclidean")

# Correlation-based distance
cor_mat  <- cor(t(dow_matrix))
dist_cor <- as.dist(1 - cor_mat)

print(round(as.matrix(dist_cor), 3))

write.csv(round(as.matrix(dist_euc), 1),
          file.path(plot_dir, "table_distance_euclidean.csv"))
write.csv(round(as.matrix(dist_cor), 3),
          file.path(plot_dir, "table_distance_correlation.csv"))


# 2. Dendrogram
hc <- hclust(dist_cor, method = "ward.D2")
clusters <- cutree(hc, k = 3)
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

# Step 3
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

pairwise_df %>%
  filter(hour == 7) %>%
  arrange(desc(abs_diff)) %>%
  head(10) %>%
  print()
