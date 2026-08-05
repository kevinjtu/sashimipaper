library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(patchwork) 

setwd("/Users/.../Sash Data")
files <- list.files(pattern = "*.xlsx")

process_file <- function(file_path) {
  # 1. Read the file
  df <- read_excel(file_path)
  
  # 2. Force the first column to be named Timestamp
  colnames(df)[1] <- "Timestamp"
  
  # 3. Robust Date Parsing
  # This tries common Excel/CSV formats: Year-Month-Day or Month/Day/Year
  df <- df %>%
    mutate(Timestamp = parse_date_time(Timestamp, orders = c("ymd HMS", "mdy HMS", "mdy HM", "ymd HM"))) %>%
    filter(!is.na(Timestamp))
  
  # 4. Clean numeric data
  # Convert all columns except Timestamp to character first, then numeric 
  # (This prevents the 'can't combine double and character' error)
  df %>%
    mutate(across(-Timestamp, ~as.numeric(as.character(.)))) %>%
    pivot_longer(-Timestamp, names_to = "Raw_ID", values_to = "CFM")
}

# 5. Combine everything
combined_data <- files %>%
  map(process_file) %>%
  bind_rows()

# 6. Clean IDs and remove overlap
final_data <- combined_data %>%
  # Keep only the Room and Hood part of the long Tridium string
  mutate(Hood_ID = str_extract(Raw_ID, "Room\\d+_FumeHood\\w+")) %>%
  # Handle the overlap: keep one unique record per Time/Hood
  distinct(Timestamp, Hood_ID, .keep_all = TRUE) %>%
  select(Timestamp, Hood_ID, CFM) %>%
  # Pivot back to your desired format
  pivot_wider(names_from = Hood_ID, values_from = CFM) %>%
  arrange(Timestamp)

# View results
print(head(final_data))

# Save the final product
write.csv(final_data, "UMD_Fume_Hood_Data.csv", row.names = FALSE)


library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# 1. Prepare the data for plotting
plot_data <- final_data %>%
  # Select only the Timestamp and the Total Exhaust columns
  select(Timestamp, contains("TotalExhaustCFM")) %>%
  # Pivot to long format: Timestamps on X, Exhaust values on Y, Room names as a grouping variable
  pivot_longer(
    cols = -Timestamp, 
    names_to = "Room_Raw", 
    values_to = "Total_Exhaust_CFM"
  ) %>%
  # Clean up the Room name (e.g., "Room1302_FumeHoodAll..." becomes "Room 1302")
  mutate(Room = str_extract(Room_Raw, "Room\\d+")) %>%
  mutate(Room = str_replace(Room, "Room", "Room "))

# 1. Define cutoff and selection
selected_rooms <- c("Room 1302", "Room 1308", "Room 2360", 
                    "Room 2364", "Room 2368", "Room 3336", "Room 3356")
cutoff <- as.POSIXct("2022-04-22")

# 2. Prepare data and periods
plot_data_stats <- plot_data %>%
  filter(Room %in% selected_rooms) %>%
  filter(!is.na(Total_Exhaust_CFM)) %>%
  mutate(Period = if_else(Timestamp < cutoff, "Before", "After"))

# 3. Calculate Period Averages
period_averages <- plot_data_stats %>%
  group_by(Room, Period) %>%
  summarise(Avg_CFM = mean(Total_Exhaust_CFM, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_text = paste0("Avg: ", round(Avg_CFM, 0), " CFM"))

# 4. Strategic Label Placement
# We'll use the min and max of the data's timeline to push text to the edges
timeline_start <- min(plot_data_stats$Timestamp)
timeline_end <- max(plot_data_stats$Timestamp)

period_averages <- period_averages %>%
  mutate(
    x_pos = if_else(Period == "Before", timeline_start, timeline_end),
    y_pos = Inf,
    # Adjust alignment: 'Before' is left-aligned, 'After' is right-aligned
    hjust_val = if_else(Period == "Before", -0.1, 1.1)
  )

# 5. Define Red Lines (4/24 and 6/6)
red_lines <- as.POSIXct(c("2022-04-22"))

room_stats <- plot_data_stats %>%
  group_by(Room) %>%
  summarise(
    # 1. P-value from independent t-test
    p_val = t.test(Total_Exhaust_CFM ~ Period)$p.value,
    
    # 2. Manual Cohen's d Calculation
    mean_before = mean(Total_Exhaust_CFM[Period == "Before"], na.rm = TRUE),
    mean_after  = mean(Total_Exhaust_CFM[Period == "After"], na.rm = TRUE),
    sd_before   = sd(Total_Exhaust_CFM[Period == "Before"], na.rm = TRUE),
    sd_after    = sd(Total_Exhaust_CFM[Period == "After"], na.rm = TRUE),
    n_before    = sum(!is.na(Total_Exhaust_CFM[Period == "Before"])),
    n_after     = sum(!is.na(Total_Exhaust_CFM[Period == "After"])),
    
    # Pooled standard deviation
    sd_pool = sqrt(((n_before - 1) * sd_before^2 + (n_after - 1) * sd_after^2) / (n_before + n_after - 2)),
    
    # Absolute effect size
    cohens_d = abs(mean_before - mean_after) / sd_pool,
    
    .groups = "drop"
  ) %>%
  mutate(
    # Format the text labels
    p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
    d_label = paste0("d = ", round(cohens_d, 2)),
    stat_label = paste(p_label, d_label, sep = "\n"), # Put p-value and d on separate lines
    
    # Coordinates to push the text to the Top-Right
    x_pos = timeline_end, 
    y_pos = Inf # Inf pushes the text to the very top of the y-axis
  )

# 6. Plotting
# 6. Plotting
ggplot(plot_data_stats, aes(x = Timestamp, y = Total_Exhaust_CFM)) +
  geom_line(color = "black", linewidth = 0.7) +
  
  # Vertical Red Lines
  geom_vline(xintercept = red_lines, color = "red", linetype = "dashed", linewidth = 0.7) +
  
  # Averages Text (Shifted to the lower margins)
  geom_text(data = period_averages, 
            aes(x = x_pos, y = y_pos, label = label_text, color = Period, hjust = hjust_val),
            vjust = 2, fontface = "bold", size = 3, show.legend = FALSE) +
  
  # NEW: P-value and Cohen's d Text (Top Right)
  geom_text(data = room_stats, 
            aes(x = x_pos, y = y_pos, label = stat_label),
            inherit.aes = FALSE, # Prevents looking for global aes mappings
            hjust = 1.05,        # Shift slightly left from the absolute right edge
            vjust = 1.5,         # Shift slightly down from the absolute top edge
            fontface = "italic", size = 3, color = "black") +
  
  facet_wrap(~ Room, scales = "free_y", ncol = 4) + 
  scale_color_manual(values = c("Before" = "blue", "After" = "darkgreen")) +
  
  labs(
    title = "Laboratory Exhaust Comparison: Pre vs. Post April 24",
    subtitle = "Red dashed lines indicate April 24 and June 6 periods",
    x = "Date",
    y = "Total Exhaust (CFM)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



library(dplyr)
library(tidyr)

# 1. Define the cutoff and selected rooms
cutoff <- as.POSIXct("2022-04-22")
selected_rooms <- c("Room 1302", "Room 1308", "Room 2360", 
                    "Room 2364", "Room 2368", "Room 3336", "Room 3356")

# 2. Calculate the drop per room
room_drop_stats <- plot_data %>%
  # Use only the selected rooms and valid data
  filter(Room %in% selected_rooms, !is.na(Total_Exhaust_CFM)) %>%
  # Categorize data into periods
  mutate(Period = if_else(Timestamp < cutoff, "Before", "After")) %>%
  # Calculate mean per room/period
  group_by(Room, Period) %>%
  summarise(Avg_CFM = mean(Total_Exhaust_CFM), .groups = "drop") %>%
  # Reshape to calculate the difference
  pivot_wider(names_from = Period, values_from = Avg_CFM) %>%
  mutate(
    CFM_Drop = Before - After,
    Percent_Reduction = (CFM_Drop / Before) * 100
  )

# 3. Calculate overall aggregate metrics
overall_metrics <- room_drop_stats %>%
  summarise(
    Mean_CFM_Drop = mean(CFM_Drop),
    Mean_Percent_Reduction = mean(Percent_Reduction)
  )

# 4. Display the results
print("--- Drop per Room ---")
print(room_drop_stats)

print("--- Overall Results ---")
cat("Average Drop across all rooms:", round(overall_metrics$Mean_CFM_Drop, 2), "CFM\n")
cat("Average Reduction percentage:", round(overall_metrics$Mean_Percent_Reduction, 2), "%\n")




# ---------------------------------------------------------
# 1. Aggregate Data: Average across all rooms per timestamp
# ---------------------------------------------------------
overall_data <- plot_data %>%
  filter(Room %in% selected_rooms) %>%
  filter(!is.na(Total_Exhaust_CFM)) %>%
  # Group by Timestamp to average all rooms at each specific time
  group_by(Timestamp) %>%
  summarise(Avg_Total_CFM = mean(Total_Exhaust_CFM, na.rm = TRUE), .groups = "drop") %>%
  # Re-apply the Before/After categorization
  mutate(Period = if_else(Timestamp < cutoff, "Before", "After"))

# ---------------------------------------------------------
# 2. Calculate Overall Period Averages
# ---------------------------------------------------------
overall_period_averages <- overall_data %>%
  group_by(Period) %>%
  summarise(Avg_CFM = mean(Avg_Total_CFM, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_text = paste0("Avg: ", round(Avg_CFM, 0), " CFM"))

# Set text positions based on the timeline
timeline_start <- min(overall_data$Timestamp)
timeline_end <- max(overall_data$Timestamp)

overall_period_averages <- overall_period_averages %>%
  mutate(
    x_pos = if_else(Period == "Before", timeline_start, timeline_end),
    y_pos = Inf,
    hjust_val = if_else(Period == "Before", -0.1, 1.1)
  )

# ---------------------------------------------------------
# 3. Calculate Overall p-value and Cohen's d
# ---------------------------------------------------------
overall_stats <- overall_data %>%
  summarise(
    # 1. P-value from independent t-test
    p_val = t.test(Avg_Total_CFM ~ Period)$p.value,
    
    # 2. Manual Cohen's d Calculation
    mean_before = mean(Avg_Total_CFM[Period == "Before"], na.rm = TRUE),
    mean_after  = mean(Avg_Total_CFM[Period == "After"], na.rm = TRUE),
    sd_before   = sd(Avg_Total_CFM[Period == "Before"], na.rm = TRUE),
    sd_after    = sd(Avg_Total_CFM[Period == "After"], na.rm = TRUE),
    n_before    = sum(!is.na(Avg_Total_CFM[Period == "Before"])),
    n_after     = sum(!is.na(Avg_Total_CFM[Period == "After"])),
    
    # Pooled standard deviation
    sd_pool = sqrt(((n_before - 1) * sd_before^2 + (n_after - 1) * sd_after^2) / (n_before + n_after - 2)),
    
    # Absolute effect size
    cohens_d = abs(mean_before - mean_after) / sd_pool,
    
    .groups = "drop"
  ) %>%
  mutate(
    p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
    d_label = paste0("d = ", round(cohens_d, 2)),
    stat_label = paste(p_label, d_label, sep = "\n"),
    x_pos = timeline_end, 
    y_pos = Inf 
  )

# ---------------------------------------------------------
# 4. Plotting the Overall Data
# ---------------------------------------------------------
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
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



library(ggplot2)
library(dplyr)

# 1. Calculate Annualized Cost per Room and Period
cost_data <- plot_data_stats %>%
  group_by(Room, Period) %>%
  summarise(Avg_CFM = mean(Total_Exhaust_CFM, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Cost = Average CFM * $7 multiplier
    Annual_Cost = Avg_CFM * 7,
    # Ensure "Before" appears first in the legend/plot
    Period = factor(Period, levels = c("Before", "After"))
  )

# 2. Create the Bar Chart
ggplot(cost_data, aes(x = Room, y = Annual_Cost, fill = Period)) +
  geom_col(position = position_dodge(width = 0.8), color = "black", size = 0.2) +
  
  # Add value labels on top of the bars for clarity
  geom_text(aes(label = paste0("$", scales::comma(round(Annual_Cost)))), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, fontface = "bold") +
  
  # Color Palette
  scale_fill_manual(values = c("Before" = "#4C72B0", "After" = "#55A868")) +
  
  # Formatting
  labs(
    title = "Annualized Operating Cost per Fume Hood",
    subtitle = "Calculated as Average CFM × $7.00 per year",
    x = "Fume Hood / Room",
    y = "Annualized Cost ($)",
    fill = "Operating Period"
  ) +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )






library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)

# ==============================================================================
# 1. Data Ingestion and Cleaning
# ==============================================================================
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-Personal/Sustainabli/Sashimi Paper/UMD Sash Data")
files <- list.files(pattern = "*.xlsx")

process_file <- function(file_path) {
  df <- read_excel(file_path)
  colnames(df)[1] <- "Timestamp"
  
  df <- df %>%
    mutate(Timestamp = parse_date_time(Timestamp, orders = c("ymd HMS", "mdy HMS", "mdy HM", "ymd HM"))) %>%
    filter(!is.na(Timestamp))
  
  df %>%
    mutate(across(-Timestamp, ~as.numeric(as.character(.)))) %>%
    pivot_longer(-Timestamp, names_to = "Raw_ID", values_to = "CFM")
}

combined_data <- files %>%
  map(process_file) %>%
  bind_rows()

final_data <- combined_data %>%
  mutate(Hood_ID = str_extract(Raw_ID, "Room\\d+_FumeHood\\w+")) %>%
  distinct(Timestamp, Hood_ID, .keep_all = TRUE) %>%
  select(Timestamp, Hood_ID, CFM) %>%
  pivot_wider(names_from = Hood_ID, values_from = CFM) %>%
  arrange(Timestamp)

# write.csv(final_data, "UMD_Fume_Hood_Data.csv", row.names = FALSE)

# ==============================================================================
# 2. Data Preparation for Plotting
# ==============================================================================
plot_data <- final_data %>%
  select(Timestamp, contains("TotalExhaustCFM")) %>%
  pivot_longer(
    cols = -Timestamp, 
    names_to = "Room_Raw", 
    values_to = "Total_Exhaust_CFM"
  ) %>%
  mutate(Room = str_extract(Room_Raw, "Room\\d+")) %>%
  mutate(Room = str_replace(Room, "Room", "Room "))

selected_rooms <- c("Room 1302", "Room 1308", "Room 2360", 
                    "Room 2364", "Room 2368", "Room 3336", "Room 3356")
cutoff <- as.POSIXct("2022-04-22")
red_lines <- as.POSIXct(c("2022-04-22"))

plot_data_stats <- plot_data %>%
  filter(Room %in% selected_rooms) %>%
  filter(!is.na(Total_Exhaust_CFM)) %>%
  mutate(Period = if_else(Timestamp < cutoff, "Before", "After"))

# ==============================================================================
# 3. Calculate Statistics (Averages, p-values, Cohen's d)
# ==============================================================================
timeline_start <- min(plot_data_stats$Timestamp, na.rm = TRUE)
timeline_end <- max(plot_data_stats$Timestamp, na.rm = TRUE)

period_averages <- plot_data_stats %>%
  group_by(Room, Period) %>%
  summarise(Avg_CFM = mean(Total_Exhaust_CFM, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    label_text = paste0("Avg: ", round(Avg_CFM, 0), " CFM"),
    x_pos = if_else(Period == "Before", timeline_start, timeline_end),
    y_pos = Inf,
    hjust_val = if_else(Period == "Before", -0.1, 1.1)
  )

room_stats <- plot_data_stats %>%
  group_by(Room) %>%
  summarise(
    p_val = t.test(Total_Exhaust_CFM ~ Period)$p.value,
    mean_before = mean(Total_Exhaust_CFM[Period == "Before"], na.rm = TRUE),
    mean_after  = mean(Total_Exhaust_CFM[Period == "After"], na.rm = TRUE),
    sd_before   = sd(Total_Exhaust_CFM[Period == "Before"], na.rm = TRUE),
    sd_after    = sd(Total_Exhaust_CFM[Period == "After"], na.rm = TRUE),
    n_before    = sum(!is.na(Total_Exhaust_CFM[Period == "Before"])),
    n_after     = sum(!is.na(Total_Exhaust_CFM[Period == "After"])),
    sd_pool = sqrt(((n_before - 1) * sd_before^2 + (n_after - 1) * sd_after^2) / (n_before + n_after - 2)),
    cohens_d = abs(mean_before - mean_after) / sd_pool,
    .groups = "drop"
  ) %>%
  mutate(
    p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
    d_label = paste0("d = ", round(cohens_d, 2)),
    stat_label = paste(p_label, d_label, sep = "\n"),
    x_pos = timeline_end, 
    y_pos = Inf 
  )

# ==============================================================================
# 4. PLOT 1: Faceted Line Chart (Time Series)
# ==============================================================================
p_lines <- ggplot(plot_data_stats, aes(x = Timestamp, y = Total_Exhaust_CFM)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_vline(xintercept = red_lines, color = "red", linetype = "solid", linewidth = 0.7) +
  
  #geom_text(data = period_averages, 
  #          aes(x = x_pos, y = y_pos, label = label_text, color = Period, hjust = hjust_val),
  #          vjust = 2, fontface = "bold", size = 3, show.legend = FALSE) +
  
  geom_text(data = room_stats, 
            aes(x = x_pos, y = y_pos, label = stat_label),
            inherit.aes = FALSE, 
            hjust = 1.05, vjust = 1.5, 
            fontface = "italic", size = 3, color = "black") +
  
  facet_wrap(~ Room, scales = "free_y", ncol = 3) + 
  #scale_color_manual(values = c("Before" = "blue", "After" = "darkgreen")) +
  
  labs(
    title = "Laboratory Exhaust Comparison: Pre vs. Post April 24",
    subtitle = "Red dashed line indicates April 24 intervention",
    x = "Date",
    y = "Total Exhaust (CFM)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_lines)

# ==============================================================================
# 5. PLOT 2: Grouped Bar Chart (Annualized Cost)
# ==============================================================================
library(dplyr)
library(ggplot2)
library(scales)
library(zoo) # Required for rollapply

# 1. Calculate the Robust Minimum Plateau Cost
rolling_window <- 6          # Assuming 6 data points = 1 hour (adjust if needed)
stability_threshold <- 15    # CFM variation threshold for a "plateau"
data_loss_floor <- 50        # CFM absolute floor to ignore sensor dropouts

min_cost_data <- plot_data_stats %>%
  arrange(Room, Timestamp) %>%
  group_by(Room) %>%
  # Remove hard dropouts
  filter(Total_Exhaust_CFM > data_loss_floor) %>%
  # Calculate rolling standard deviation to find stable periods
  mutate(
    rolling_sd = rollapply(Total_Exhaust_CFM, width = rolling_window, FUN = sd, fill = NA, align = "right")
  ) %>%
  # Keep only the stable plateaus
  filter(rolling_sd < stability_threshold) %>%
  # Find the 5th percentile (Robust Min Plateau)
  summarise(
    Robust_Min_Plateau = quantile(Total_Exhaust_CFM, 0.05, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Format to match the cost_data structure
  mutate(
    Period = "Minimum",
    Annual_Cost = Robust_Min_Plateau * 7
  ) %>%
  select(Room, Period, Annual_Cost)

# 2. Calculate the Average Cost (Before/After)
avg_cost_data <- plot_data_stats %>%
  group_by(Room, Period) %>%
  summarise(Avg_CFM = mean(Total_Exhaust_CFM, na.rm = TRUE), .groups = "drop") %>%
  mutate(Annual_Cost = Avg_CFM * 7)

# 3. Combine both into one dataset and set the factor levels
cost_data <- bind_rows(avg_cost_data, min_cost_data) %>%
  mutate(
    # Ensure the legend and bars appear in this specific order
    Period = factor(Period, levels = c("Before", "After", "Minimum"))
  )

# 4. Create the Bar Chart
# 4. Create the Bar Chart (Fixed Overlap)
p_bars <- ggplot(cost_data, aes(x = Room, y = Annual_Cost, fill = Period)) +
  # ADDED: width = 0.7 to the bars so they fit inside the 0.8 dodge width
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.2) +
  
  # Ensure the text dodges by the exact same 0.8 width to stay centered over the bars
  geom_text(aes(label = paste0("$", scales::comma(round(Annual_Cost)))), 
            position = position_dodge(width = 0.8), angle = 45,
            size = 3, fontface = "bold") +
  
  # Color Palette: Added Grey for the Minimum Baseline
  scale_fill_manual(values = c("Before" = "#4C72B0", "After" = "#55A868", "Minimum" = "#8C8C8C")) +
  
  # Formatting
  labs(
    title = "Annualized Operating Cost per Room",
    subtitle = "Calculated as Average CFM (and Minimum Baseline) × $7.00 per year",
    x = "Room",
    y = "Annualized Cost ($)",
    fill = "Operating Period"
  ) +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_bars)


