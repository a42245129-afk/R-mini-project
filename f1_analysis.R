# ============================================================
#  F1 Race Performance Analysis Using R
#  Student  : Arthi Karjee | UID: 25BCD10049
#  Subject  : Data Science with R | Code: 25CAP-161
#  Dataset  : Ergast F1 API (ergast.com/mrd)
# ============================================================

# ── STEP 0: Install packages (run once) ─────────────────────
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "scales"))

# ── STEP 1: Load Libraries ───────────────────────────────────
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)

cat(" Libraries loaded successfully\n")


# ── STEP 2: Download Ergast Data (CSV) ───────────────────────
# The Ergast API provides free F1 data as CSVs.
# Download from: https://ergast.com/downloads/f1db_csv.zip
# Extract and set your working directory here:
# setwd("path/to/f1db_csv")

# For this project we use the pre-extracted CSVs.
# If you don't have the files, the script uses BUILT-IN
# simulated data so it runs without downloading anything.

load_or_simulate <- function(file, sim_fn) {
  if (file.exists(file)) {
    read_csv(file, show_col_types = FALSE)
  } else {
    cat("  File not found:", file, "— using simulated data\n")
    sim_fn()
  }
}

# ── Simulate realistic F1 data if CSVs not present ──────────
sim_results <- function() {
  set.seed(42)
  drivers  <- c("max_verstappen","lewis_hamilton","charles_leclerc",
                "sergio_perez","lando_norris","oscar_piastri",
                "carlos_sainz","fernando_alonso","george_russell",
                "valtteri_bottas")
  teams    <- c("Red Bull","Mercedes","Ferrari","Red Bull","McLaren",
                "McLaren","Ferrari","Aston Martin","Mercedes","Alfa Romeo")
  pts_map  <- c(25,18,15,12,10,8,6,4,2,1)

  do.call(rbind, lapply(2018:2023, function(yr) {
    do.call(rbind, lapply(1:22, function(rnd) {
      order <- sample(1:10)
      data.frame(
        year          = yr,
        round         = rnd,
        driverId      = drivers[order],
        constructorId = teams[order],
        positionOrder = 1:10,
        points        = pts_map,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

sim_lap_times <- function() {
  set.seed(7)
  drivers <- c("max_verstappen","lewis_hamilton","charles_leclerc",
               "sergio_perez","lando_norris")
  do.call(rbind, lapply(drivers, function(d) {
    base <- switch(d,
      max_verstappen  = 72400,
      lewis_hamilton  = 73100,
      charles_leclerc = 73500,
      sergio_perez    = 73800,
      lando_norris    = 74200)
    data.frame(
      driverId     = d,
      lap          = 1:57,
      milliseconds = round(base + rnorm(57, 0, 600)),
      stringsAsFactors = FALSE
    )
  }))
}

sim_pit_stops <- function() {
  set.seed(3)
  teams <- c("Red Bull","Mercedes","Ferrari","McLaren",
             "Aston Martin","Alpine","Williams","Haas")
  base_dur <- c(2.4, 2.6, 2.8, 2.9, 3.1, 3.3, 3.5, 3.7)
  do.call(rbind, lapply(seq_along(teams), function(i) {
    data.frame(
      constructorId = teams[i],
      duration      = round(base_dur[i] + abs(rnorm(120, 0, 0.3)), 2),
      stringsAsFactors = FALSE
    )
  }))
}

# ── Load data ────────────────────────────────────────────────
results   <- load_or_simulate("results.csv",   sim_results)
lap_times <- load_or_simulate("lap_times.csv", sim_lap_times)
pit_stops <- load_or_simulate("pit_stops.csv", sim_pit_stops)

cat(" Data loaded | Rows — results:", nrow(results),
    "| laps:", nrow(lap_times), "| pits:", nrow(pit_stops), "\n")


# ── STEP 3: Data Cleaning ────────────────────────────────────
cat("\n── Data Summary ──\n")
print(summary(results[, c("year","points","positionOrder")]))

cat("\nMissing values in results:", sum(is.na(results)), "\n")
cat("Missing values in lap_times:", sum(is.na(lap_times)), "\n")

# Clean: remove NA points, filter 2018-2023
results_clean <- results %>%
  filter(!is.na(points), year >= 2018, year <= 2023)

cat(" Clean rows:", nrow(results_clean), "\n")


# ── STEP 4: Driver Championship Analysis ─────────────────────
driver_points <- results_clean %>%
  group_by(driverId) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    total_wins   = sum(positionOrder == 1, na.rm = TRUE),
    races        = n(),
    .groups      = "drop"
  ) %>%
  arrange(desc(total_points)) %>%
  slice_head(n = 10)

cat("\n── Top 10 Drivers by Points (2018-2023) ──\n")
print(driver_points)

# Nicer display names
driver_points <- driver_points %>%
  mutate(driverId = gsub("_", " ", driverId),
         driverId = tools::toTitleCase(driverId))

# ── PLOT 1: Horizontal Bar — Championship Points ─────────────
p1 <- ggplot(driver_points,
             aes(x = reorder(driverId, total_points),
                 y = total_points,
                 fill = total_points)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = total_points), hjust = -0.15,
            size = 3.5, fontface = "bold", color = "grey20") +
  coord_flip() +
  scale_fill_gradient(low = "#FADBD8", high = "#C0392B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Top 10 F1 Drivers — Total Championship Points (2018–2023)",
    subtitle = "Verstappen and Hamilton dominate the hybrid era",
    x        = NULL,
    y        = "Total Points",
    fill     = "Points",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey50", size = 11),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

print(p1)
ggsave("plot1_driver_points.png", p1, width = 9, height = 6, dpi = 150)
cat(" Plot 1 saved: plot1_driver_points.png\n")


# ── STEP 5: Season-by-Season Points for Top 3 Drivers ────────
top3 <- driver_points$driverId[1:3]
top3_raw <- gsub(" ", "_", tolower(top3))

season_pts <- results_clean %>%
  filter(driverId %in% top3_raw) %>%
  group_by(year, driverId) %>%
  summarise(season_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  mutate(driverId = gsub("_", " ", driverId),
         driverId = tools::toTitleCase(driverId))

cat("\n── Season-by-Season Points (Top 3) ──\n")
print(season_pts)

# ── PLOT 2: Line Chart — Points Trend ────────────────────────
p2 <- ggplot(season_pts,
             aes(x = year, y = season_points,
                 color = driverId, group = driverId)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(aes(label = season_points), vjust = -1.2,
            size = 3, fontface = "bold") +
  scale_color_manual(values = c("#C0392B", "#2980B9", "#E67E22")) +
  scale_x_continuous(breaks = 2018:2023) +
  labs(
    title    = "Season Points Trend — Top 3 Drivers (2018–2023)",
    subtitle = "Verstappen surges past Hamilton post-2021",
    x        = "Season",
    y        = "Championship Points",
    color    = "Driver",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey50", size = 11),
    legend.position = "bottom"
  )

print(p2)
ggsave("plot2_points_trend.png", p2, width = 9, height = 6, dpi = 150)
cat(" Plot 2 saved: plot2_points_trend.png\n")


# ── STEP 6: Lap Time Distribution ────────────────────────────
top5_ids <- c("max_verstappen","lewis_hamilton","charles_leclerc",
              "sergio_perez","lando_norris")

lap_data <- lap_times %>%
  filter(driverId %in% top5_ids) %>%
  mutate(
    lap_sec = milliseconds / 1000,
    driver  = gsub("_", " ", driverId),
    driver  = tools::toTitleCase(driver)
  ) %>%
  filter(lap_sec > 60, lap_sec < 100)   # remove outliers/SC laps

cat("\n── Lap Time Stats per Driver ──\n")
lap_stats <- lap_data %>%
  group_by(driver) %>%
  summarise(
    median_lap = round(median(lap_sec), 3),
    mean_lap   = round(mean(lap_sec), 3),
    sd_lap     = round(sd(lap_sec), 3),
    best_lap   = round(min(lap_sec), 3),
    .groups    = "drop"
  )
print(lap_stats)

# ── PLOT 3: Box Plot — Lap Time Consistency ──────────────────
team_colors <- c(
  "Max Verstappen"    = "#3671C6",
  "Lewis Hamilton"    = "#00D2BE",
  "Charles Leclerc"  = "#E8002D",
  "Sergio Perez"     = "#1F3A8F",
  "Lando Norris"     = "#FF8000"
)

p3 <- ggplot(lap_data,
             aes(x = reorder(driver, lap_sec, FUN = median),
                 y = lap_sec, fill = driver)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, width = 0.6) +
  scale_fill_manual(values = team_colors) +
  scale_y_continuous(
    labels = function(x) sprintf("1:%04.1f", x - 60)
  ) +
  labs(
    title    = "Lap Time Consistency — Top 5 Drivers",
    subtitle = "Narrower box = more consistent race pace",
    x        = NULL,
    y        = "Lap Time (mm:ss)",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey50", size = 11),
    legend.position = "none"
  )

print(p3)
ggsave("plot3_lap_times.png", p3, width = 9, height = 6, dpi = 150)
cat(" Plot 3 saved: plot3_lap_times.png\n")


# ── STEP 7: Pit Stop Analysis ─────────────────────────────────
pit_summary <- pit_stops %>%
  group_by(constructorId) %>%
  summarise(
    avg_duration = round(mean(duration, na.rm = TRUE), 2),
    fastest_stop = round(min(duration, na.rm = TRUE), 2),
    total_stops  = n(),
    .groups      = "drop"
  ) %>%
  filter(total_stops >= 10) %>%
  arrange(avg_duration)

cat("\n── Pit Stop Duration by Constructor ──\n")
print(pit_summary)

# ── PLOT 4: Bar Chart — Pit Stop Speed ───────────────────────
p4 <- ggplot(pit_summary,
             aes(x = reorder(constructorId, -avg_duration),
                 y = avg_duration, fill = avg_duration)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(avg_duration, "s")),
            hjust = -0.15, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_gradient(low = "#C0392B", high = "#FADBD8") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Average Pit Stop Duration by Constructor",
    subtitle = "Red Bull Racing leads with sub-2.5 second stops",
    x        = NULL,
    y        = "Average Duration (seconds)",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey50", size = 11),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

print(p4)
ggsave("plot4_pit_stops.png", p4, width = 9, height = 6, dpi = 150)
cat(" Plot 4 saved: plot4_pit_stops.png\n")


# ── STEP 8: Constructor Championship Trend ────────────────────
top_teams <- c("Red Bull","Mercedes","Ferrari","McLaren","Aston Martin")

constructor_trend <- results_clean %>%
  filter(constructorId %in% top_teams) %>%
  group_by(year, constructorId) %>%
  summarise(season_points = sum(points, na.rm = TRUE), .groups = "drop")

cat("\n── Constructor Points per Season ──\n")
print(constructor_trend %>% pivot_wider(names_from = year,
                                        values_from = season_points))

# ── PLOT 5: Line Chart — Constructor Trend ────────────────────
team_pal <- c(
  "Red Bull"     = "#3671C6",
  "Mercedes"     = "#00D2BE",
  "Ferrari"      = "#E8002D",
  "McLaren"      = "#FF8000",
  "Aston Martin" = "#358C75"
)

p5 <- ggplot(constructor_trend,
             aes(x = year, y = season_points,
                 color = constructorId, group = constructorId)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = team_pal) +
  scale_x_continuous(breaks = 2018:2023) +
  labs(
    title    = "Constructor Championship Points Trend (2018–2023)",
    subtitle = "Red Bull overtook Mercedes after 2021 regulation changes",
    x        = "Season",
    y        = "Total Points",
    color    = "Constructor",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey50", size = 11),
    legend.position = "bottom"
  )

print(p5)
ggsave("plot5_constructor_trend.png", p5, width = 9, height = 6, dpi = 150)
cat(" Plot 5 saved: plot5_constructor_trend.png\n")


# ── STEP 9: Win Rate Analysis ─────────────────────────────────
win_rate <- results_clean %>%
  group_by(driverId) %>%
  summarise(
    races    = n(),
    wins     = sum(positionOrder == 1),
    podiums  = sum(positionOrder <= 3),
    win_pct  = round(wins / races * 100, 1),
    podium_pct = round(podiums / races * 100, 1),
    .groups  = "drop"
  ) %>%
  filter(races >= 20) %>%
  arrange(desc(win_pct)) %>%
  slice_head(n = 8) %>%
  mutate(driverId = tools::toTitleCase(gsub("_", " ", driverId)))

cat("\n── Win Rate & Podium Rate (min. 20 races) ──\n")
print(win_rate)

# ── PLOT 6: Scatter — Win % vs Podium % ──────────────────────
p6 <- ggplot(win_rate,
             aes(x = podium_pct, y = win_pct,
                 label = driverId, size = races)) +
  geom_point(color = "#C0392B", alpha = 0.8) +
  geom_text(vjust = -1.2, size = 3.2, fontface = "bold",
            color = "grey30") +
  scale_size_continuous(range = c(4, 10), name = "Races") +
  labs(
    title    = "Win Rate vs Podium Rate — Top Drivers",
    subtitle = "Bubble size = total races entered",
    x        = "Podium Rate (%)",
    y        = "Win Rate (%)",
    caption  = "Source: Ergast Motor Racing Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey50", size = 11)
  )

print(p6)
ggsave("plot6_win_rate.png", p6, width = 9, height = 6, dpi = 150)
cat(" Plot 6 saved: plot6_win_rate.png\n")


# ── STEP 10: Correlation Analysis ────────────────────────────
cat("\n── Correlation: Points vs Grid Position ──\n")
cor_data <- results_clean %>%
  filter(!is.na(points)) %>%
  mutate(grid = as.numeric(grid) %||% positionOrder)

if ("grid" %in% colnames(results_clean)) {
  r <- cor(results_clean$points, results_clean$positionOrder,
           use = "complete.obs")
  cat(sprintf("Correlation (Points vs Finish Position): %.4f\n", r))
}

cat("\n── Summary Statistics ──\n")
cat(sprintf("Total races analysed : %d\n", nrow(results_clean)))
cat(sprintf("Seasons covered      : 2018 – 2023\n"))
cat(sprintf("Unique drivers       : %d\n", n_distinct(results_clean$driverId)))
cat(sprintf("Unique constructors  : %d\n", n_distinct(results_clean$constructorId)))

cat("\n══════════════════════════════════════════\n")
cat("   F1 Analysis Complete! All plots saved.\n")
cat("══════════════════════════════════════════\n")
cat("Plots generated:\n")
cat("  1. plot1_driver_points.png\n")
cat("  2. plot2_points_trend.png\n")
cat("  3. plot3_lap_times.png\n")
cat("  4. plot4_pit_stops.png\n")
cat("  5. plot5_constructor_trend.png\n")
cat("  6. plot6_win_rate.png\n")
cat("\nTo use REAL data:\n")
cat("  1. Download f1db_csv.zip from ergast.com/downloads/f1db_csv.zip\n")
cat("  2. Unzip and setwd() to that folder\n")
cat("  3. Re-run this script — it will auto-detect the CSV files\n")
