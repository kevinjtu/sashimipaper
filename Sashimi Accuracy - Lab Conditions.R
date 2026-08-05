# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(readxl)

# PART 1: Accuracy Analysis (First Sheet)####
# 1. Read and Prepare Data
setwd("/Users/.../data")
sensor_data <- read_excel("Sensor Accuracy Data.xlsx", sheet = 2)

long_data <- sensor_data %>%
  pivot_longer(
    cols = -`Actual Height`, 
    names_to = "Sensor_Option", 
    values_to = "Measured_Height"
  )

# Force the specific order
long_data$Sensor_Option <- factor(long_data$Sensor_Option, 
                                  levels = c("0 mm", "3 mm", "6 mm (calibrated)", "9 mm", "12 mm"))

# 2. Calculate Stats (Accuracy Tolerance <= 1 inch)
stats_df <- long_data %>%
  group_by(Sensor_Option) %>%
  summarize(
    R_Value = cor(`Actual Height`, Measured_Height, use = "complete.obs"),
    P_Value = cor.test(`Actual Height`, Measured_Height)$p.value,
    Accuracy = base::mean(abs(`Actual Height` - Measured_Height) <= 1)
  ) %>%
  mutate(
    Label = sprintf("R = %.3f\nP = %.3g\nAccuracy within 1 in = %.1f%%", R_Value, P_Value, Accuracy * 100)
  )

# 3. Separate Data for the Two Plots
# Set A: The Calibrated Sensor
data_calib <- filter(long_data, Sensor_Option == "6 mm (calibrated)")
stats_calib <- filter(stats_df, Sensor_Option == "6 mm (calibrated)")

# Set B: The Other Sensors (0, 3, 9, 12)
data_others <- filter(long_data, Sensor_Option != "6 mm (calibrated)")
stats_others <- filter(stats_df, Sensor_Option != "6 mm (calibrated)")

# 4. Create Plot 1: Calibrated Only (Text Bottom Right)
p1 <- ggplot(data_calib, aes(x = `Actual Height`, y = Measured_Height)) +
  geom_point(alpha = 0.5, size = 3, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_text(
    data = stats_calib, 
    aes(label = Label), 
    # Move to Bottom Right: Max X, Min Y
    x = max(data_calib$`Actual Height`), 
    y = min(data_calib$Measured_Height), 
    hjust = 1, # Align right edge of text to the x coordinate
    vjust = 0, # Align bottom edge of text to the y coordinate
    size = 4
  ) +
  labs(title = "Calibrated Sensor (6 mm)", x = "Actual", y = "Measured") +
  theme_bw()

# 5. Create Plot 2: Others 2x2 Grid (Text Bottom Right)
p2 <- ggplot(data_others, aes(x = `Actual Height`, y = Measured_Height)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_text(
    data = stats_others, 
    aes(label = Label), 
    # Move to Bottom Right: Max X, Min Y
    x = max(data_others$`Actual Height`), 
    y = min(data_others$Measured_Height), 
    hjust = 1, 
    vjust = 0, 
    size = 3
  ) +
  facet_wrap(~ Sensor_Option, ncol = 2) + 
  labs(title = "Sensitvity Analysis", x = "Actual", y = "Measured") +
  theme_bw()

# 6. Display Plots
print(p1)
print(p2)

# PART 2: Residual plots Analysis (First Sheet) ####

# 1. Read and Prepare Data
sensor_data <- read_excel("Sensor Accuracy Data.xlsx", sheet = 2) %>%
  select(-`6 mm (calibrated)`)  # Remove Trial ID for clarity

long_data <- sensor_data %>%
  pivot_longer(
    cols = -`Actual Height`, 
    names_to = "Sensor_Option", 
    values_to = "Measured_Height"
  )

# Force the specific order
long_data$Sensor_Option <- factor(long_data$Sensor_Option, 
                                  levels = c("0 mm", "3 mm", "6 mm (calibrated)", "9 mm", "12 mm"))

# 2. Calculate Stats (Accuracy Tolerance <= 1 inch)
stats_df <- long_data %>%
  group_by(Sensor_Option) %>%
  summarize(
    R_Value = cor(`Actual Height`, Measured_Height, use = "complete.obs"),
    P_Value = cor.test(`Actual Height`, Measured_Height)$p.value,
    Accuracy = base::mean(abs(`Actual Height` - Measured_Height) <= 1)
  ) %>%
  mutate(
    Label = sprintf("R = %.3f\nP = %.3g\nAccuracy within 1 in = %.1f%%", R_Value, P_Value, Accuracy * 100)
  )

# 3. Separate Data for the Two Plots
# Set A: The Calibrated Sensor
data_calib <- filter(long_data, Sensor_Option == "6 mm (calibrated)")
stats_calib <- filter(stats_df, Sensor_Option == "6 mm (calibrated)")

# Set B: The Other Sensors (0, 3, 9, 12)
data_others <- filter(long_data, Sensor_Option != "6 mm (calibrated)")
stats_others <- filter(stats_df, Sensor_Option != "6 mm (calibrated)")

# 4. Create Plot 1: Calibrated Only (Text Bottom Right)
p1 <- ggplot(data_calib, aes(x = `Actual Height`, y = Measured_Height)) +
  geom_point(alpha = 0.5, size = 3, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_text(
    data = stats_calib, 
    aes(label = Label), 
    # Move to Bottom Right: Max X, Min Y
    x = max(data_calib$`Actual Height`), 
    y = min(data_calib$Measured_Height), 
    hjust = 1, # Align right edge of text to the x coordinate
    vjust = 0, # Align bottom edge of text to the y coordinate
    size = 4
  ) +
  labs(title = "Calibrated Sensor (6 mm)", x = "Actual", y = "Measured") +
  theme_bw()

# 5. Create Plot 2: Others 2x2 Grid (Text Bottom Right)
p2 <- ggplot(data_others, aes(x = `Actual Height`, y = Measured_Height)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_text(
    data = stats_others, 
    aes(label = Label), 
    # Move to Bottom Right: Max X, Min Y
    x = max(data_others$`Actual Height`), 
    y = min(data_others$Measured_Height), 
    hjust = 1, 
    vjust = 0, 
    size = 3
  ) +
  facet_wrap(~ Sensor_Option, ncol = 2) + 
  labs(title = "Sensitvity Analysis", x = "Actual", y = "Measured") +
  theme_bw()

# 6. Display Plots
print(p1)
print(p2)# 1. Prepare Data (assuming 'long_data' is already loaded from previous steps)
# If not, re-run the "reshape" block from the previous turn
data_residuals <- long_data %>%
  mutate(
    # Calculate the Error (Residual)
    Error = Measured_Height - `Actual Height`,
    
    # Label for clarity in the plot
    Sensor_Label = paste0(Sensor_Option) 
  )

# 2. Plot Residuals
ggplot(data_residuals, aes(x = `Actual Height`, y = Error)) +
  # Add a reference line at 0 (Perfect Accuracy)
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  
  # Add dashed lines for +/- 1 inch tolerance
  geom_hline(yintercept = c(-1, 1), color = "gray50", linetype = "dashed") +
  
  # Plot the error points
  geom_point(aes(color = Sensor_Option), alpha = 0.6, size = 2) +
  
  # Add trend lines (Linear) to show the "Systematic" nature
  geom_smooth(method = "lm", se = FALSE, aes(color = Sensor_Option), size = 0.8) +
  
  facet_wrap(~ Sensor_Option) +
  
  scale_y_continuous(breaks = seq(-10, 10, 2)) +
  
  labs(
    title = "Systematic Error Analysis (Residuals)",
    subtitle = "Clean lines indicate predictable offsets rather than random noise",
    x = "Actual Height (in)",
    y = "Measurement Error (in)"
  ) +
  theme_bw() +
  theme(legend.position = "none") # Hide legend since titles are enough
