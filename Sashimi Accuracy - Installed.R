# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readxl)

# 1. Load the data
# Replace with the actual file path or name
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-Personal/Sustainabli/Sashimi Paper")
data <- read_excel("Sashimi Accuracy Data - Installed.xlsx", sheet = 2)

# Clean column names to make them easier to work with
colnames(data) <- c("Actual", "Recorded", "Hood")

# 2. Calculate the deviation (Recorded - Actual)
data <- data %>%
  mutate(Deviation = Recorded - Actual,
         Hood = as.factor(Hood))

# 3. Calculate Accuracy Metrics for Annotations

# Calculate accuracy for <= 16 (Within 1 inch) per hood
anno_working <- data %>%
  filter(Actual <= 16) %>%
  group_by(Hood) %>%
  summarise(
    Acc = sum(abs(Deviation) <= 1) / n() * 100,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("<= 16 in Accuracy\n(Within 1\"): ", round(Acc, 1), "%"),
    x = 8, # Position text horizontally at x=8
    y = max(data$Deviation, na.rm = TRUE) * 0.9 # Position text dynamically near the top
  )

# Calculate accuracy for > 16 (Recorded > 16) per hood
anno_high <- data %>%
  filter(Actual > 16) %>%
  group_by(Hood) %>%
  summarise(
    Acc = sum(Recorded > 16, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("> 16 in Accuracy\n(Read > 16\"): ", round(Acc, 1), "%"),
    x = 20, # Position text horizontally at x=20
    y = max(data$Deviation, na.rm = TRUE) * 0.9 # Position text dynamically near the top
  )

# 4. Generate the Plot
p <- ggplot(data, aes(x = Actual, y = Deviation)) +
  # Add the data points
  geom_point(aes(color = Hood), size = 3, alpha = 0.7) +
  
  # Create separate plots for each hood
  facet_wrap(~Hood, labeller = label_both) +
  
  # Add a horizontal line at 0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Add a vertical line at 16 to separate the ranges
  geom_vline(xintercept = 16, linetype = "dotted", color = "red", size = 1) +
  
  # Add text annotations for <= 16 range
  geom_text(data = anno_working, aes(x = x, y = y, label = label), 
            size = 3.5, fontface = "bold", color = "darkblue", inherit.aes = FALSE) +
  
  # Add text annotations for > 16 range
  geom_text(data = anno_high, aes(x = x, y = y, label = label), 
            size = 3.5, fontface = "bold", color = "darkred", inherit.aes = FALSE) +
  
  # Labeling
  labs(title = "Sensor Accuracy: Deviation from Actual Measurement",
       subtitle = "Red line separates Working Range (<= 16 in) and High Range (> 16 in)",
       x = "Actual Sash Height (in)",
       y = "Recorded - Actual (in)") +
  
  # Adjust y-axis slightly higher to ensure text fits without clipping
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) + 
  theme_bw() +
  theme(legend.position = "none") # Hide redundant legend since facets label the hoods

# Display the plot
print(p)