# Load in libraries
# Be sure they are installed first
library(tidyverse)
library(hrbrthemes)
library(gt)
library(ggplot2)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library(scales)

# NOTE: We use two datasets, one involving occupational data,
# another dealing w/international violence

# Read in occupational data-set:
data2 <- read.csv("occs.csv")

# Change variables:
attach(data2)
Occupation <- as.factor(Occupation)
SOC <- as.factor(SOC)

# Cumsum the change bar widths
data2$right <- cumsum(data2$Hires) + 30*c(0:(nrow(data2)-1))
data2$left <- data2$right - data2$Hires

# Make actual var-width barplot:
ggplot(data2, aes(ymin = 0)) + 
   geom_rect(aes(xmin = left, xmax = right, ymax = Earnings, colour = Occupation, fill = Occupation)) +
   xlab("Count (#) of Hires") + 
   ylab("$/per Hour") +
   ggtitle("Number of Hires vs. Wages in DFW (June 2024)") +
   theme_ipsum() +
   theme(legend.position="bottom",
         axis.title.x = element_text(size = 12, hjust = 0.5),
         axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 2.5),
         plot.title = element_text(hjust = 0.5),
         legend.key.size = unit(0.004, 'cm'),
         legend.key.height = unit(0.006, 'cm'),
         legend.key.width = unit(0.006, 'cm'),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"))



# New graphs using international incident data-set:
  

# Load international dataset:
data <- read.csv("oneside.csv")

# India fat. chart (an extra chart):
# Filter data
india_data <- data %>%
  filter(location == "India")

# Summ. estimates 
summary_india <- india_data %>%
  group_by(year) %>%
  summarise(
    avg_fatality = mean(best_fatality_estimate, na.rm = TRUE),
    low_fatality = mean(low_fatality_estimate, na.rm = TRUE),
    high_fatality = mean(high_fatality_estimate, na.rm = TRUE)
  )

# Create a basic plot
summary_long <- summary_india %>%
  pivot_longer(cols = c(avg_fatality, low_fatality, high_fatality),
               names_to = "fatality_type",
               values_to = "fatality")

# Make enhanced plot 
ggplot(summary_long, aes(x = year, y = fatality, color = fatality_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_smooth(aes(group = fatality_type), method = "loess", se = FALSE, linetype = "dotted") +  # Add smooth lines
  labs(title = "Fatality Estimates in India Over the Years",
       subtitle = "Average, Low, and High Fatality Rates",
       x = "Year",
       y = "Fatality Estimates",
       color = "Fatality Type") +
  theme_minimal(base_size = 15) +  # Increase base font size
  scale_color_manual(values = c("avg_fatality" = "steelblue", 
                                "low_fatality" = "forestgreen", 
                                "high_fatality" = "red4")) +  # Use specified colors
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12, angle = 45, hjust = 1)  # Slant x-axis labels
  ) +
  scale_x_continuous(breaks = seq(min(summary_long$year), max(summary_long$year), by = 1))  # Customize x-axis breaks

# Multi-Chart:
# List countries
selected_countries <- c("India", "Pakistan", "Afghanistan", "Nigeria", 
                        "Yemen", "Iraq", "Burundi", "Sierra Leonne", "Colombia", 
                        "Philippines", "Mexico")

# Table w/ embedded charts:

# Filter data 
filtered_data <- data %>%
  filter(location %in% selected_countries)

# Summ. fatality 
summary_data <- filtered_data %>%
  group_by(location, year) %>%
  summarise(
    avg_fatality = mean(best_fatality_estimate, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = avg_fatality,
               names_to = "fatality_type",
               values_to = "fatality")

# Combine plots
ggplot(summary_data, aes(x = year, y = fatality, color = location, group = location)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Fatality Estimates by Country Over the Years",
       subtitle = "Selected Countries",
       x = "Year",
       y = "Average Fatality Estimates",
       color = "Country") +
  theme_minimal(base_size = 15) +
  scale_color_brewer(palette = "Set1") +  # Use a distinct color palette
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12, angle = 45, hjust = 1)
  ) +
  facet_wrap(~ location, scales = "free_y")  # Separate plots for each country with free y-scales


# A Barchart:

# reload international dataset as new df
mydata <- read.csv("oneside.csv")
head(mydata)
str(mydata)

# List new countries
selected_countries <- c("India", "Pakistan", "Afghanistan", "Nigeria", 
                        "Yemen", "Iraq", "Burundi", "Sierra Leone", 
                        "Colombia", "Philippines", "Mexico")

# Filter 
filtered_data <- mydata %>%
  filter(location %in% selected_countries)

# Summarize
summary_data <- filtered_data %>%
  group_by(location) %>%
  summarise(
    avg_fatality = mean(best_fatality_estimate, na.rm = TRUE),
    low_fatality = mean(low_fatality_estimate, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avg_fatality, low_fatality),
               names_to = "fatality_type",
               values_to = "fatality")

# Make chart 
ggplot(summary_data, aes(x = location)) +
  geom_bar(aes(y = fatality, fill = fatality_type), 
           position = position_dodge(width = 0.6),  # Adjust width for overlap
           stat = "identity", width = 0.6, color = "black") +  # Set bar width
  labs(title = "Fatality Estimates by Country",
       x = "Country",
       y = "Fatality Estimates",
       fill = "Fatality Type") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("avg_fatality" = "steelblue", 
                               "low_fatality" = "forestgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Slant x-axis labels

# Save option:
ggsave("fatality_estimates_plot.png", width = 10, height = 6)

