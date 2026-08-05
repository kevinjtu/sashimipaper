library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Load the data
setwd("/Users/.../datafolder")
# Load necessary libraries

# Load your data (replace with your actual file path if needed)
data <- read_csv("data.csv")

# ==============================================================================
# Step 1: Calculate the CFM from each fume hood
# ==============================================================================
data_calculated <- data %>%
  mutate(timestamp = mdy_hm(timestamp)) %>%
  filter(timestamp >= as.POSIXct("2025-03-01") & timestamp <= as.POSIXct("2025-06-15")) %>%
  mutate(
    # A. Calculate Flow Constants
    min_airflow = hood_width * max_sash_height * closed_velocity_i,
    max_airflow = hood_width * max_sash_height * open_velocity_i,
    
    # B. Calculate Percentage Open
    p_open_i = current_sash_height / max_sash_height,
    
    # C. Calculate Final CFM (Linear Sash Interpolation)
    hood_cfm = (p_open_i * (max_airflow - min_airflow)) + min_airflow
  ) %>%
  # Drop any rows where timestamp failed to parse
  filter(!is.na(timestamp))

# ==============================================================================
# Step 2: Average the CFM for each fume hood into hourly bins
# ==============================================================================
hourly_hood_data <- data_calculated %>%
  mutate(hour_bin = floor_date(timestamp, unit = "hour")) %>%
  group_by(hour_bin, room_id, hood_id) %>%
  summarize(avg_hood_cfm = mean(hood_cfm, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# Step 3: Find each room's total CFM for each hourly timepoint
# ==============================================================================
hourly_room_data <- hourly_hood_data %>%
  group_by(hour_bin, room_id) %>%
  summarize(room_total_cfm = sum(avg_hood_cfm, na.rm = TRUE), .groups = "drop") %>%
  # Create the Before/After categorization for April 5, 2025
  mutate(period = ifelse(hour_bin < as.POSIXct("2025-04-05"), "Before", "After"))

# ==============================================================================
# Step 4: Overall average CFM plot and Stats
# ==============================================================================
# Calculate overall average of all rooms for each timepoint
overall_hourly <- hourly_room_data %>%
  group_by(hour_bin, period) %>%
  summarize(overall_avg_cfm = mean(room_total_cfm, na.rm = TRUE), .groups = "drop")

# 4a & 4b: Find Before/After averages, P-value, and Cohen's d
overall_stats <- overall_hourly %>%
  group_by(period) %>%
  summarize(mean_cfm = mean(overall_avg_cfm, na.rm = TRUE))

mean_before <- overall_stats$mean_cfm[overall_stats$period == "Before"]
mean_after <- overall_stats$mean_cfm[overall_stats$period == "After"]

# T-test & Cohen's d (Overall)
t_test_overall <- t.test(overall_avg_cfm ~ period, data = overall_hourly)
cohen_overall <- cohen.d(overall_avg_cfm ~ as.factor(period), data = overall_hourly)

# Create Annotation string
overall_annot <- paste0(
  "Before Avg: ", round(mean_before, 2), " CFM\n",
  "After Avg: ", round(mean_after, 2), " CFM\n",
  "P-value: ", signif(t_test_overall$p.value, 3), "\n",
  "Cohen's d: ", round(cohen_overall$estimate, 3)
)

# Plot Overall
p_overall <- ggplot(overall_hourly, aes(x = hour_bin, y = overall_avg_cfm)) +
  geom_line(color = "black", linewidth = 1) +
  geom_vline(xintercept = as.POSIXct("2025-04-05"), color = "red", linetype = "dashed", linewidth = 0.7) +
  # Adding stats text to the top left of the plot
  annotate("text", x = min(overall_hourly$hour_bin, na.rm = TRUE), 
           y = max(overall_hourly$overall_avg_cfm, na.rm = TRUE), 
           label = overall_annot, hjust = 0, vjust = 1, fontface = "bold") +
  theme_bw() +
  labs(
    title = "Overall Average Room CFM Over Time",
    x = "Date",
    y = "Average Room CFM"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_overall)


ggplot(overall_data, aes(x = Timestamp, y = Avg_Total_CFM)) +
  geom_line(color = "black", linewidth = 1) +
  
  # Vertical Red Lines (Assuming 'red_lines' is already defined in your environment)
  geom_vline(xintercept = red_lines, color = "red", linetype = "dashed", linewidth = 0.7) +
  
  # Averages Text
  geom_text(data = overall_period_averages, 
            aes(x = x_pos, y = y_pos, label = label_text, color = Period, hjust = hjust_val),
            vjust = 2, fontface = "bold", size = 5, show.legend = FALSE) +
  
  # P-value and Cohen's d Text (Top Right)
  geom_text(data = overall_stats, 
            aes(x = x_pos, y = y_pos, label = stat_label),
            inherit.aes = FALSE, 
            hjust = 1.05,        
            vjust = 1.5,         
            fontface = "italic", size = 5, color = "black") +
  
  scale_color_manual(values = c("Before" = "blue", "After" = "darkgreen")) +
  
  labs(
    title = "Overall Laboratory Exhaust Comparison: Pre vs. Post April 24",
    subtitle = "Averaged across all selected rooms. Red dashed lines indicate April 24 and June 6 periods.",
    x = "Date",
    y = "Average Total Exhaust (CFM)"
  ) +
  theme_bw() 




# ==============================================================================
# Step 5: Stratify by Room (Same Steps)
# ==============================================================================
# Plot stratified by room using facet_wrap
p_stratified <- ggplot(hourly_room_data, aes(x = hour_bin, y = room_total_cfm)) +
  geom_line(color = "black", linewidth = 1) +
  geom_vline(xintercept = as.POSIXct("2025-04-05"), color = "red", linetype = "solid", size = 0.7) +
  facet_wrap(~ room_id, scales = "free_y") + # Allows different y-axes for different rooms
  theme_minimal() +
  labs(
    title = "Total Room CFM Over Time, Stratified by Room",
    x = "Date",
    y = "Room CFM"
  )

print(p_stratified)

# Calculate Before/After stats, P-value, and Cohen's d for EACH room
# Because printing text on a multi-pane faceted plot gets crowded, 
# we output the stats into a clean data frame so you can view it directly.
room_statistics <- hourly_room_data %>%
  group_by(room_id) %>%
  # Filter out rooms that don't have enough data in both periods to run a t-test
  filter(n_distinct(period) == 2, min(table(period)) >= 2) %>%
  summarize(
    mean_before = mean(room_total_cfm[period == "Before"], na.rm = TRUE),
    mean_after = mean(room_total_cfm[period == "After"], na.rm = TRUE),
    p_value = t.test(room_total_cfm ~ period)$p.value,
    cohens_d = cohen.d(room_total_cfm ~ as.factor(period))$estimate,
    .groups = "drop"
  )

print("--- Room Stratified Statistics ---")
print(room_statistics)







# Load necessary libraries
library(tidyverse)
library(lubridate)
library(effsize)
library(scales)

# Load your data
data <- read_csv("UPENN_export_corrected v2 demo data.csv")

# ==============================================================================
# Step 1: Calculate the CFM from each fume hood
# ==============================================================================
data_calculated <- data %>%
  mutate(timestamp = mdy_hm(timestamp)) %>%
  # Filter data to be between Feb 25, 2025 and June 1, 2025
  filter(timestamp >= as.POSIXct("2025-02-25") & timestamp <= as.POSIXct("2025-06-01")) %>%
  mutate(
    min_airflow = hood_width * max_sash_height * closed_velocity_i,
    max_airflow = hood_width * max_sash_height * open_velocity_i,
    p_open_i = current_sash_height / max_sash_height,
    hood_cfm = (p_open_i * (max_airflow - min_airflow)) + min_airflow
  ) %>%
  filter(!is.na(timestamp))

# ==============================================================================
# Step 2 & 3: Hourly bins and Room Totals
# ==============================================================================
hourly_hood_data <- data_calculated %>%
  mutate(hour_bin = floor_date(timestamp, unit = "hour")) %>%
  group_by(hour_bin, room_id, hood_id) %>%
  summarize(avg_hood_cfm = mean(hood_cfm, na.rm = TRUE), .groups = "drop")

hourly_room_data <- hourly_hood_data %>%
  group_by(hour_bin, room_id) %>%
  summarize(room_total_cfm = sum(avg_hood_cfm, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = ifelse(hour_bin < as.POSIXct("2025-04-05"), "Before", "After"))

# ==============================================================================
# Step 4: Calculate Statistics for Line Charts
# ==============================================================================
room_statistics <- hourly_room_data %>%
  group_by(room_id) %>%
  filter(n_distinct(period) == 2, min(table(period)) >= 2) %>%
  summarize(
    mean_before = mean(room_total_cfm[period == "Before"], na.rm = TRUE),
    mean_after = mean(room_total_cfm[period == "After"], na.rm = TRUE),
    p_value = t.test(room_total_cfm ~ period)$p.value,
    cohens_d = cohen.d(room_total_cfm ~ as.factor(period))$estimate,
    .groups = "drop"
  ) %>%
  mutate(
    # Format the stats for the line chart annotation
    stat_label = paste0("p = ", signif(p_value, 3), "\nd = ", round(cohens_d, 2))
  )

# Calculate dynamic text placement (top right of each facet)
facet_annotations <- hourly_room_data %>%
  group_by(room_id) %>%
  summarize(
    max_date = max(hour_bin, na.rm = TRUE),
    max_cfm = max(room_total_cfm, na.rm = TRUE) * 1.05, # Push slightly above max line
    .groups = "drop"
  ) %>%
  inner_join(room_statistics, by = "room_id")

# ==============================================================================
# Step 5: Calculate Annualized Costs (Before, After, Minimum)
# ==============================================================================
# A. Average Cost (Before/After)
avg_cost_data <- hourly_room_data %>%
  group_by(room_id, period) %>%
  summarize(Avg_CFM = mean(room_total_cfm, na.rm = TRUE), .groups = "drop") %>%
  mutate(Annual_Cost = Avg_CFM * 7)

# B. Minimum Cost (Sum of min_airflow for all hoods in the room * 7)
min_cost_data <- data_calculated %>%
  group_by(room_id, hood_id) %>%
  summarize(min_airflow = first(min_airflow), .groups = "drop") %>%
  group_by(room_id) %>%
  summarize(min_room_cfm = sum(min_airflow, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    period = "Minimum", 
    Annual_Cost = min_room_cfm * 7
  ) %>%
  select(room_id, period, Annual_Cost)

# C. Combine into one Cost DataFrame and apply Room 230 Exception
cost_data <- bind_rows(avg_cost_data, min_cost_data) %>%
  mutate(
    # Ensure the factor order puts Minimum at the end
    period = factor(period, levels = c("Before", "After", "Minimum")),
    
    # SPECIAL CASE: Force Annual_Cost to $4900 for room 230
    Annual_Cost = ifelse(room_id == "230", 4900, Annual_Cost)
  ) %>%
  # Filter to only keep rooms that exist in our valid statistical testing pool
  filter(room_id %in% room_statistics$room_id)

# ==============================================================================
# Step 6: Plot 1 - Faceted Line Chart
# ==============================================================================
p_lines <- ggplot(hourly_room_data %>% filter(room_id %in% room_statistics$room_id), 
                  aes(x = hour_bin, y = room_total_cfm)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_vline(xintercept = as.POSIXct("2025-04-05"), color = "red", linetype = "solid", linewidth = 0.7) +
  # Add stats text to each facet
  geom_text(data = facet_annotations, 
            aes(x = max_date, y = max_cfm, label = stat_label),
            hjust = 1, vjust = 1, fontface = "italic", size = 3, color = "black") +
  facet_wrap(~ room_id, scales = "free_y", ncol = 3) +
  labs(
    title = "Time Series: Fume Hood Exhaust (CFM) by Room",
    subtitle = "Red dashed line indicates intervention (April 5, 2025)",
    x = "Date",
    y = "Total Exhaust (CFM)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(p_lines)

# ==============================================================================
# Step 7: Plot 2 - Grouped Bar Chart for Costs
# ==============================================================================
p_costs <- ggplot(cost_data, aes(x = room_id, y = Annual_Cost, fill = period, group = period)) +
  
  # Bars
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.2) +
  
  
  # Value Labels above bars (tilted 45°)
  geom_text(
    aes(label = paste0("$", scales::comma(round(Annual_Cost)))),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    hjust = 0,
    angle = 45,
    size = 3,
    fontface = "bold"
  ) +
  
  # Color Palette
  scale_fill_manual(values = c(
    "Before" = "#4C72B0",
    "After" = "#55A868",
    "Minimum" = "#8C8C8C"
  )) +
  
  # Labels
  labs(
    title = "Annualized Operating Cost by Room",
    subtitle = "Calculated as Average CFM (or Minimum Baseline) × $7.00 per year",
    x = "Room ID",
    y = "Annualized Cost ($)",
    fill = "Operating Period"
  ) +
  
  # Y-axis formatting
  scale_y_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.3))
  ) +
  
  # Theme
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_costs)





