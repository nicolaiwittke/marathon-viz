# AI disclosure:
# ChatGPT was used as a coding assistant for parts of the data preparation,
# including handling Excel time formats and parsing split times (marked in code).
# All visualization design choices, analysis decisions, and interpretations
# were developed independently.

#setwd("~/Documents/marathon-viz")

# Required packages
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(scales)
library(tidyr)

women_file   <- "data_raw/nyc_women.xls"
men_file     <- "data_raw/nyc_men.xls"
chicago_file <- "data_raw/chicago.xlsx"
boston_file  <- "data_raw/boston.xlsx"



#1.1 New York Marathon
nyc_women_small <- read_excel(women_file, skip = 3) |>
  clean_names() |>
  transmute(
    place      = overall_place,
    name       = trimws(name),
    final_time = as.character(overall_time),
    gender     = "W",
    city       = "New York"
  )

nyc_men_small <- read_excel(men_file, skip = 3) |>
  clean_names() |>
  transmute(
    place      = overall_place,
    name       = trimws(name),
    final_time = as.character(overall_time),
    gender     = "M",
    city       = "New York"
  )

nyc_small <- bind_rows(nyc_women_small, nyc_men_small)

# 1.2 Chicago Marathon
chicago_male <- read_excel(chicago_file, sheet = "Male") |>
  clean_names()

chicago_female <- read_excel(chicago_file, sheet = "Female") |>
  clean_names()

chicago_male_small <- chicago_male |>
  transmute(
    place = place,
    name  = trimws(name),
    final_time = {
      x <- finishing_time
      if (inherits(x, c("POSIXct", "POSIXt"))) { #most common excel date problem handling we have observed
        format(x, "%H:%M:%S")
      } else {
        as.character(x)
      }
    },
    gender = "M",
    city   = "Chicago"
  )

chicago_female_small <- chicago_female |>
  transmute(
    place = place,
    name  = trimws(name),
    final_time = {
      x <- finishing_time
      if (inherits(x, c("POSIXct", "POSIXt"))) {
        format(x, "%H:%M:%S")
      } else {
        as.character(x)
      }
    },
    gender = "W",
    city   = "Chicago"
  )

chicago_small <- bind_rows(chicago_male_small, chicago_female_small)


## 1.3 Boston Marathon
boston_women <- read_excel(boston_file, sheet = "Top 100 Women") |>
  clean_names()

boston_men <- read_excel(boston_file, sheet = "Top 100 Men") |>
  clean_names()

boston_women_small <- boston_women |>
  transmute(
    place = place,
    name  = trimws(name),
    final_time = {
      x <- final_time
      if (inherits(x, c("POSIXct", "POSIXt"))) {
        format(x, "%H:%M:%S")
      } else {
        as.character(x)
      }
    },
    gender = "W",
    city   = "Boston"
  )

boston_men_small <- boston_men |>
  transmute(
    place = place,
    name  = trimws(name),
    final_time = {
      x <- final_time
      if (inherits(x, c("POSIXct", "POSIXt"))) {
        format(x, "%H:%M:%S")
      } else {
        as.character(x)
      }
    },
    gender = "M",
    city   = "Boston"
  )

boston_small <- bind_rows(boston_women_small, boston_men_small)


#1.4 All Marathons combined 
finishers_total_small <- bind_rows(nyc_small, chicago_small, boston_small)
names(finishers_total_small) <- c("Place", "Name", "Final Time", "Gender", "City")

# checks
#count(finishers_total_small, City, Gender) # -> 100 each -> passed 

#2 Data Visualization

#plot 1

df <- finishers_total_small %>%
  mutate(
    sex = case_when(
      Gender %in% c("M","Male","Men") ~ "Men",
      Gender %in% c("W","Female","Women") ~ "Women",
      TRUE ~ as.character(Gender)
    ),
    # help of GPT for correct time coding
    ft = str_trim(as.character(`Final Time`)),
    h = as.numeric(str_extract(ft, "^[0-9]+")),
    m = as.numeric(str_extract(ft, "(?<=:)[0-9]{2}(?=:)")),
    s = as.numeric(str_extract(ft, "[0-9]{2}$")),
    finish_minutes = h*60 + m + s/60
  ) %>%
  filter(!is.na(finish_minutes), finish_minutes > 0, sex %in% c("Men","Women")) %>%
  mutate(city = factor(City, levels = c("Boston","Chicago","New York")))

fmt_hms <- function(mins) {
  secs <- round(mins * 60)
  hh <- secs %/% 3600
  mm <- (secs %% 3600) %/% 60
  ss <- secs %% 60
  sprintf("%d:%02d:%02d", hh, mm, ss)
}

cols <- c("Men" = "#1f77b4", "Women" = "#d62728")
binwidth_min <- 1.5 # can be changed freely, no particular reason for 1.5, could be 2 as well

spread_df <- df %>%
  group_by(city, sex) %>%
  summarise(
    q25 = quantile(finish_minutes, 0.25, na.rm = TRUE),
    med = quantile(finish_minutes, 0.50, na.rm = TRUE),
    q75 = quantile(finish_minutes, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

n_df <- df %>% count(city, sex, name = "N")
df <- df %>% left_join(n_df, by = c("city","sex"))

# consistent y-axis limits across all graphs and cities:
ymax_df <- df %>%
  mutate(bin = floor(finish_minutes / binwidth_min) * binwidth_min) %>% #help of GPT to define the bins
  count(city, bin, name = "n") %>%
  group_by(city) %>%
  summarise(ymax = max(n), .groups = "drop")

spread_df <- spread_df %>% left_join(ymax_df, by = "city")

iqr_band <- spread_df %>% # help of GPT to define the optimal IQR band
  transmute(
    city, sex,
    xmin = q25, xmax = q75,
    ymin = 0,
    ymax = ymax * 0.92
  )

ny_note <- tibble(
  city = factor("New York", levels = levels(df$city)),
  x = 160,   
  y = 20,   
  label = "distinct clusters\n(not just wider spread)"
)


p1 <- ggplot(df, aes(x = finish_minutes)) +
  
  geom_rect(
    data = iqr_band,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sex),
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  
  geom_histogram(
    aes(fill = sex),
    binwidth = binwidth_min,
    position = "identity",
    alpha = 0.14, 
    color = NA
  ) +
  
  geom_density(
    aes(
      color = sex,
      y = after_stat(density * n * binwidth_min) # Use of GPT so the density lines up with the count and doesn't integrate to 1 only (nicer visuals)
    ),
    linewidth = 1.0,
    adjust = 1.1
  ) +
  
  geom_vline(
    data = spread_df,
    aes(xintercept = med, color = sex),
    linewidth = 1.3 
  ) +
  
  geom_text(
    data = ny_note,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.4,
    fontface = "italic",
    alpha = 0.7
  ) +
  
  facet_wrap(~ city, ncol = 1, scales = "fixed") +
  
  scale_fill_manual(values = cols) +
  
  scale_color_manual(values = cols) +
  
  scale_x_continuous(
    labels = function(x) fmt_hms(x),
    breaks = pretty_breaks(n = 6)
  ) +
  
  labs(
    title = "Top-100 Marathon Finish Times by Sex (2025)",
    subtitle = "Elite finish-time distributions show similar central density across courses, but New York exhibits distinct clustering rather than simple spread.",
    x = "Finish time (H:MM:SS)",
    y = "Count",
    fill = NULL,
    color = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

print(p1)



#plot 2 - polar heatmap

#p2.1 Helper function generated with help of GPT -> it is being used to get the cumulative seconds for the mile intervals to 
# get information about the speed per mile 
parse_to_seconds <- function(x) {
  x <- str_trim(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  n_colons <- str_count(x, ":")
  
  out <- rep(NA_real_, length(x))
  
  # MM:SS
  idx2 <- which(!is.na(x) & n_colons == 1)
  if (length(idx2) > 0) {
    mm <- as.numeric(str_extract(x[idx2], "^[0-9]+"))
    ss <- as.numeric(str_extract(x[idx2], "(?<=:)[0-9]{2}$"))
    out[idx2] <- mm * 60 + ss
  }
  
  # H:MM:SS
  idx3 <- which(!is.na(x) & n_colons >= 2)
  if (length(idx3) > 0) {
    hh <- as.numeric(str_extract(x[idx3], "^[0-9]+"))
    mm <- as.numeric(str_extract(x[idx3], "(?<=:)[0-9]{2}(?=:)"))
    ss <- as.numeric(str_extract(x[idx3], "[0-9]{2}$"))
    out[idx3] <- hh * 3600 + mm * 60 + ss
  }
  
  out
}

#p2.2 Full Data for the NYC Marathon including time stamps per mile
nyc <- bind_rows(
  read_excel(women_file, skip = 3) |> clean_names() |> mutate(sex = "Women"),
  read_excel(men_file,   skip = 3) |> clean_names() |> mutate(sex = "Men")
)

mile_cols <- names(nyc)[str_detect(names(nyc), "^x\\d+m_split$")]

nyc_long <- nyc |>
  select(sex, name, all_of(mile_cols)) |>
  pivot_longer(cols = all_of(mile_cols), names_to = "mile_marker", values_to = "t_raw") |>
  mutate(
    mile  = as.numeric(str_extract(mile_marker, "\\d+")),
    t_sec = parse_to_seconds(t_raw) # here we use the GPT generated function to get the cumulative seconds for mile intervals
  ) |>
  filter(!is.na(mile)) |>
  arrange(sex, name, mile)

# add start (0 miles) so that we can get a speed for first 3 miles
start_rows <- nyc_long |>
  distinct(sex, name) |>
  mutate(mile = 0, t_sec = 0)

nyc_long2 <- bind_rows(nyc_long, start_rows) |>
  arrange(sex, name, mile)

#p2.3 defining the intervals and interval bins for the grouping of male and female athletes' pace respectively
intervals <- nyc_long2 |>
  group_by(sex, name) |>
  arrange(mile, .by_group = TRUE) |>
  mutate(
    prev_mile = lag(mile),
    prev_t    = lag(t_sec),
    dmile     = mile - prev_mile,
    dt_sec    = t_sec - prev_t,
    pace_min_per_mile = (dt_sec / dmile) / 60,
    interval_label = paste0(prev_mile, "–", mile)
  ) |>
  ungroup() |>
  filter(!is.na(pace_min_per_mile), dmile > 0)

interval_bins <- intervals |>
  distinct(prev_mile, mile, interval_label) |>
  arrange(prev_mile, mile) |>
  mutate(
    dmile = mile - prev_mile,
    interval_label_pretty = if_else(dmile > 1 | interval_label == "25–26", paste0(interval_label, "*"), interval_label)
  )

interval_order <- interval_bins$interval_label
interval_labels_pretty <- interval_bins$interval_label_pretty

#p2.4 here we normalize the pace for each sex group, the reason being, we don't want to look at differences between sex in terms
#of pace but looking at the relative pace profile !
polar_df <- intervals |>
  group_by(sex, interval_label) |>
  summarize(mean_pace = mean(pace_min_per_mile, na.rm = TRUE), .groups = "drop") |>
  group_by(sex) |>
  mutate(
    rel_pace = (mean_pace - mean(mean_pace)) / mean(mean_pace) #important here is that <0 means faster than average and >0 slower than average
  ) |>
  ungroup() |>
  mutate(
    interval_label = factor(interval_label, levels = interval_order),
    sex = factor(sex, levels = c("Women", "Men"))
  )

#p2.5 here we attach the relative pace info and labels to the ring geometry which we define and define women in inner ring and men in outer ring
ring_geom <- tibble(
  sex = factor(c("Women", "Men"), levels = c("Women", "Men")),
  y0  = c(3.2, 4.25),
  y1  = c(4.05, 5.10)
)

polar_df2 <- polar_df |>
  left_join(ring_geom, by = "sex") |>
  mutate(
    y = (y0 + y1) / 2,
    height = y1 - y0
  )

#p2.6 We used GPT to parse data on climb and descent information from the following website: "https://runready.substack.com/p/the-2025-tcs-new-york-marathon-course"
#and for a clear mapping we decided to keep only clear climbs or descents within one interval and do not handle mixed events in the same interval 

interval_bins_1mi <- interval_bins |> filter(abs(dmile - 1) < 1e-9)

mile_to_interval_1mi <- function(m) {
  idx <- which(interval_bins_1mi$prev_mile <= m & interval_bins_1mi$mile > m)
  if (length(idx) == 0) return(NA_character_)
  interval_bins_1mi$interval_label[max(idx)]
}

markers <- tibble(
  mile = c(
    0.21, 0.62, 13.02, 14.78, 15.19, 19.46, 23.46, 24.69,
    1.04, 1.35, 1.66, 8.56, 11.16, 13.33, 15.71, 19.97, 24.38, 25.10
  ),
  type = c(rep("Climb", 8), rep("Descent", 10))
) |>
  mutate(interval_label = vapply(mile, mile_to_interval_1mi, character(1))) |>
  filter(!is.na(interval_label)) |>
  group_by(interval_label) |>
  filter(n_distinct(type) == 1) |>
  slice(1) |>
  ungroup() |>
  mutate(
    interval_label = factor(interval_label, levels = interval_order),
    type = factor(type, levels = c("Climb", "Descent")),
    y = max(ring_geom$y1) + 0.12
  )

#p2.7 Start Point
start_label <- interval_order[1]

start_tick <- tibble(
  interval_label = factor(start_label, levels = interval_order),
  y0 = max(ring_geom$y1) + 0.01,
  y1 = max(ring_geom$y1) + 0.24 
)

start_text <- tibble(
  interval_label = factor(start_label, levels = interval_order),
  y = max(ring_geom$y1) + 0.33,
  label = "Start"
)

#p2.8 Legend
pace_labeller <- function(x) {
  ifelse(
    abs(x) < 1e-10, "avg**",
    ifelse(x < 0,
           paste0(abs(round(x * 100)), "% faster"),
           paste0(abs(round(x * 100)), "% slower"))
  )
}

#p2.9 Plot2
keep_idx <- seq(1, length(interval_order), by = 2) #using only every second interval otherwise it might be too crowded

p2 <- ggplot(polar_df2, aes(x = interval_label, y = y, fill = rel_pace)) +
  
  geom_tile(aes(height = height), width = 1, color = "white", linewidth = 0.20) +
  
  geom_hline(
    yintercept = mean(c(ring_geom$y1[1], ring_geom$y0[2])),
    linewidth = 0.5,
    color = "grey90"
  ) +
  
  geom_segment(
    data = start_tick,
    aes(x = interval_label, xend = interval_label, y = y0, yend = y1),
    inherit.aes = FALSE,
    linewidth = 1.1,
    color = "grey15",
    lineend = "round"
  ) +
  
  geom_point(
    data = start_tick,
    aes(x = interval_label, y = y0),
    inherit.aes = FALSE,
    size = 1.8,
    color = "grey15"
  ) +
  
  geom_text(
    data = start_text,
    aes(x = interval_label, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "bold",
    color = "grey15"
  ) +
  
  geom_point(
    data = markers,
    aes(x = interval_label, y = y, shape = type),
    inherit.aes = FALSE,
    size = 2.8,
    color = "grey10",
    fill = "grey10"
  ) +
  
  annotate(
    "text",
    x = interval_order[1],
    y = 2.4,
    label = "Men (outer)\n&\nWomen (inner)",
    size = 4,
    fontface = "bold",
    color = "grey40",
    hjust = 0.5
  ) +
  
  coord_polar(theta = "x", start = 2*pi) +
  
  scale_fill_gradient2(
    low = "#2b8cbe",
    mid = "white",
    high = "#d7301f",
    midpoint = 0,
    name = "Interval pace vs. sex average",
    breaks = c(-0.02, 0, 0.02, 0.05),
    labels = pace_labeller,
    guide = guide_colorbar(reverse = TRUE)  # faster on top, slower on bottom
  ) +
  
  scale_shape_manual(
    name = "Course profile",
    values = c(Climb = 24, Descent = 25),
    labels = c("Uphill (climb)", "Downhill (descent)")
  ) +
  
  guides(
    fill  = guide_colorbar(order = 1, reverse = TRUE),
    shape = guide_legend(
      order = 2,
      override.aes = list(color = "grey10", fill = "grey10", size = 2.8)
    )
  ) +
  
  scale_y_continuous(
    limits = c(2.4, max(ring_geom$y1) + 0.38),
    breaks = NULL
  ) +
  
  scale_x_discrete(
    breaks = interval_order[keep_idx],
    labels = interval_labels_pretty[keep_idx]
  ) +
  
  labs(
    title = "Men and Women Pace the NYC Marathon in Similar Ways",
    subtitle = "Normalized to sex-specific averages · Intervals shown in miles",
    caption = "* Aggregated intervals due to split availability\n**Avg pace: Men 5:49/mi, Women 6:36/mi (Top-100 finisher)",
    x = NULL, y = NULL
  ) +
  
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size=11, hjust = 0, margin = margin(b=10), lineheight=1.2),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(size = 10, margin = margin(t = 4)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

p2

ggsave("output/plot1_finish_times.png", p1, width = 15, height = 9, dpi = 300)
ggsave("output/plot2_polar_pace.png",  p2, width = 10.5, height = 8, dpi = 300, bg ="white")


