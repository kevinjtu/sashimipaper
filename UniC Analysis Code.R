library(data.table)
library(lubridate)
library(janitor)
library(ggplot2)


setwd("~/datafolder")

stanf_data <- as.data.frame(fread("sash data.csv"))
room_mapping <- as.data.frame(fread("Fume Hood Info.csv"))

room_mapping <- room_mapping %>%
  clean_names()

stanf_data <- stanf_data %>%
  mutate(
    fume_hood_name = str_extract(fume_hood_name, "\\d+"), # Extract digits from names like Test(117)
    fume_hood_name = as.character(fume_hood_name)
  ) %>% 
  filter(fume_hood_name != "117") 
  
room_mapping <- room_mapping %>%
  mutate(fume_hood_name = as.character(fume_hood_name))

merged <- merge(
  stanf_data,
  room_mapping,
  by = "fume_hood_name",
  all.x = TRUE
)

merged <- merged %>%
  mutate(
    time = parse_date_time(time, orders = "mdy HM"),
    percent_open = sash_height / (sash_height_ft * 12) * 100
  )

# Define date cutoffs
start_after_period <- ymd_hms("2025-04-20 00:00:00")
end_after_period <- ymd_hms("2025-05-22 23:59:59")

# Filter by time
before_april_20 <- merged %>% filter(time < start_after_period)
after_april_20 <- merged %>% filter(time >= start_after_period & time <= end_after_period)

# Summarize for each period
summary_before <- before_april_20 %>%
  group_by(labs) %>%
  summarise(
    avg_percent_open_before = mean(percent_open, na.rm = TRUE),
    .groups = "drop"
  )

summary_after <- after_april_20 %>%
  group_by(labs) %>%
  summarise(
    avg_percent_open_after = mean(percent_open, na.rm = TRUE),
    .groups = "drop"
  )

# Combine results
final_summary <- full_join(summary_before, summary_after, by = "labs")

# Add difference columns
final_summary <- final_summary %>%
  mutate(
    change_percent_open = avg_percent_open_after - avg_percent_open_before
  )

# View result
print(final_summary)


# Define vertical line dates
line1 <- ymd_hms("2025-04-20 00:00:00")
line2 <- ymd_hms("2025-05-22 23:59:59")

# Plot
ggplot(merged, aes(x = time, y = percent_open)) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = as.numeric(line1), linetype = "solid", color = "blue") +
  geom_vline(xintercept = as.numeric(line2), linetype = "solid", color = "red") +
  facet_wrap(~ fume_hood_name, scales = "free_x") +
  labs(
    title = "Percent Open Over Time by Fume Hood",
    x = "Time",
    y = "Percent Open"
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

write.csv(final_summary, "final_summary.csv", row.names = FALSE)
