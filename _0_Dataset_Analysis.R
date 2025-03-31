# ==============================================================================
#                           DATA VISUALISATION
# Heat alerts + Temperatures + Mortality
#
# Author: Mahulena Koristkova
# Last update: 31/03/2025
# ==============================================================================

library(ggplot2)
library(viridis)
library(lubridate) 
library(dplyr) 
library(tseries) 
library(forecast)
library(caret)
library(patchwork)
library(tidyr)

# Expected Dataset Attributes
# For Heat Alert Data:
# cityname, year, date, hw (hw == 1 for heat alert days, NA otherwise)
# For Meteo Data: 
# cityname, year, month, date, tmax, tmin, tmean
# For Mortality Data:
# date, mortality, cityname
# attribute date is expected to be in "YYYY-MM-DD" format

ha_data <- read.csv("PL_merged_data.csv")
implementation_year <- 2009 # Set the year of HEWS implementation

ha_data$date <- as.Date(ha_data$date)

cities <- unique(ha_data$cityname)
years <- unique(ha_data$year)

# ------------------------------------------------------------------------------
#                            HEAT ALERT DATA
# ------------------------------------------------------------------------------

# Create a summary table in long format: year vs. cityname vs. number of HAs
ha_summary <- ha_data %>%
  filter(hw == 1) %>%
  group_by(year, cityname) %>%
  summarise(entry_count = n(), .groups = "drop")

# ------------------------------------------------------------------------------

# This part deals with years that did not register any HAs
# The following code explicitly adds these rows:
# year - cityname - 0
# So that in the final plot, all bars have the same width

all_years <- unique(ha_summary$year)
all_cities <- unique(ha_summary$cityname)

# Create a complete grid of all year-cityname combinations
full_grid <- expand.grid(year = all_years, cityname = all_cities)

# Add missing combinations with entry_count = 0
ha_summary <- full_grid %>%
  left_join(ha_summary, by = c("year", "cityname")) %>%
  mutate(entry_count = ifelse(is.na(entry_count), 0, entry_count))

# ------------------------------------------------------------------------------

# BAR PLOT
# Adjust y-scale according to need

{
plot <- ggplot(ha_summary, aes(x = year, y = entry_count, fill = cityname)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, colour="white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "viridis") +
  labs(
    title = "Annual Number of Heat Alerts per City",
    x = NULL,
    y = "No. of Alert Days",
    fill = NULL
  ) +
  scale_x_continuous(
    breaks = unique(ha_summary$year)
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size=12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.2, hjust=0.3)
  )

plot
}

# --- SAVE
# ggsave(
#   filename = "HA_No_per_year.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white"
# )

# ------------------------------------------------------------------------------

rm(full_grid, all_cities, all_years, plot)

# ------------------------------------------------------------------------------
#                                 METEO DATA
# ------------------------------------------------------------------------------

meteo <- data.frame(date = ha_data$date,
                    tmean = ha_data$tmean,
                    tmax = ha_data$tmax,
                    tmin = ha_data$tmin,
                    cityname = ha_data$cityname,
                    year = ha_data$year,
                    month = as.factor(ha_data$month)
                    )

# ------------------------------------------------------------------------------

# Compare distributions of maximum temperatures before and after implementation
# Check distribution similarity via Kolmogorov-Smirnov test
# Adjust wrap plot nrows and ncols according to need
# The plots have no legend. Blue = Pre-Implementation, Red = Post-Implementation

meteo_1 <- subset(meteo, year < implementation_year)
meteo_2 <- subset(meteo, year >= implementation_year)

plots_list <- list()

{
for (city in cities) {
  
  tmean_1 <- meteo_1$tmean[meteo_1$cityname == city]
  tmean_2 <- meteo_2$tmean[meteo_2$cityname == city]
  
  # Run KS test
  ks_result <- ks.test(tmean_1, tmean_2)
  
  # ---------------------------------------------------------
  
  # Subset data for the city
  meteo_1_city <- meteo_1[meteo_1$cityname == city, ]
  meteo_2_city <- meteo_2[meteo_2$cityname == city, ]
  
  # Histogram Plot
  histogram_plot <- ggplot() +
    geom_histogram(data = meteo_1_city, aes(x = tmean, fill = "Meteo 1"), 
                   alpha = 0.5, bins = 20, color = "black") +
    geom_histogram(data = meteo_2_city, aes(x = tmean, fill = "Meteo 2"), 
                   alpha = 0.5, bins = 20, color = "black") +
    scale_fill_manual(
      name = "Dataset",  
      values = c("Meteo 1" = "#1f78b4", "Meteo 2" = "#e31a1c"),
      labels = c("Pre-Implementation", "Post-Implementation")
    ) +
    labs(title = paste(city, ", K-S test p-value < 0.05, D:", round(ks_result$statistic, 3)), x = "T(mean) (°C)", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA))
  
  # CDF Plot
  cdf_plot <- ggplot() +
    stat_ecdf(data = meteo_1_city, aes(x = tmean, color = "Meteo 1"), linewidth = 0.5) +
    stat_ecdf(data = meteo_2_city, aes(x = tmean, color = "Meteo 2"), linewidth = 0.5) +
    scale_color_manual(
      name = "Dataset",  
      values = c("Meteo 1" = "#1f78b4", "Meteo 2" = "#e31a1c"),
      labels = c("Pre-Implementation", "Post-Implementation")
    ) +
    labs(x = "T(mean) (°C)", y = "Cum. Probability") +
    theme_minimal() +
    theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA))
  
  # Combine histogram and CDF side by side for this city
  city_plot <- histogram_plot + cdf_plot + plot_layout(ncol = 2)
  
  # Store in list
  plots_list[[city]] <- city_plot
}

combined_plot <- wrap_plots(plots_list) + plot_layout(ncol = 2, nrow = 5)
print(combined_plot)
}

# --- SAVE
# ggsave(
#   filename = "Tmean_Dist.png",
#   plot = combined_plot,
#   width = 16, 
#   height = 12,
#   dpi = 300,
#   bg = "white"
# )

rm(cdf_plot, city_plot, combined_plot, meteo_1, meteo_2, meteo_1_city, 
   meteo_2_city, plots_list, ks_results, histogram_plot, tmean_1, tmean_2,
   ks_results)
# ------------------------------------------------------------------------------

# Time Series Plot: Wrapped by year
# Tmax + Tmean + Tmin
# x-ticks break after 4 years, adjust according to need
# The limits for x-ticks have to be set explicitly, otherwise "empty" years
# are included

{
  plot <- ggplot(meteo, aes(x = date)) +
    geom_line(aes(y = tmax, color = "tmax"), linewidth = 0.3, alpha = 0.9) +
    geom_line(aes(y = tmin, color = "tmin"), linewidth = 0.3, alpha = 0.9) +
    geom_line(aes(y = tmean, color = "tmean"), linewidth = 0.3, alpha = 0.9) +
    scale_color_manual(
      name = NULL,  # Legend title
      values = c("tmax" = "#e31a1c", "tmin" = "#1f78b4", "tmean" = "gray10"),
      labels = c("Max Temp (°C)", "Mean Temp (°C)", "Min Temp (°C)")
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = "Temperature (°C)"
    ) +
    scale_x_date(date_breaks = "4 year", 
                 date_labels = "%Y",
                 limits = as.Date(c('1991-01-01','2020-12-31')),
                 expand = c(0,0)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size=12),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.2, hjust=0.3),
      strip.text = element_text(size=16, face="bold", hjust = 0)
    ) +
    facet_wrap(. ~ cityname, ncol = 1, scales = "free_y")
  
  
  print(plot)
}


# --- SAVE
# ggsave(
#   filename = "Temp_All.png",
#   plot = plot,
#   width = 8, 
#   height = 12,
#   dpi = 300,
#   bg = "white"
# )


# ------------------------------------------------------------------------------

# Boxplots for City vs. Month vs. Tmean
# Seasonality visualization

{
  plot <- ggplot(meteo, aes(x = month)) +
    geom_boxplot(aes(y = tmean), outlier.shape = NA, alpha = 1, fill = "gray") +
    labs(
      title = NULL,
      x = "Month",
      y = "Temperature (°C)"
    ) +
    theme_minimal() +
    theme(
      legend.text = element_text(size=12),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.2, hjust=0.3),
      strip.text = element_text(size=16, face="bold", hjust = 0)
    ) +
    facet_wrap(. ~ cityname, ncol = 1, scales = "free_y")
  
  
  print(plot)
  
}

# ------------------------------------------------------------------------------

# Grid plot for Monthly Temperature Distributions during Warm months
# City - Specific
# 3 Columns: tmax, tmean, tmin
# Rows = Cities

# Extract month from date and only include warm months
meteo$month <- format(meteo$date, "%m") 
meteo_month <- subset(meteo, month == c("05", "06", "07", "08", "09"))

# Long format, supports faceting
meteo_long <- meteo_month %>%
  select(cityname, month, tmin, tmax, tmean) %>%
  pivot_longer(cols = c(tmin, tmax, tmean), names_to = "temp_type", values_to = "temperature")

{
plot <- ggplot(meteo_long, aes(x = factor(month), y = temperature, fill = temp_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  scale_fill_manual(values = c("tmin" = "gray", "tmax" = "gray", "tmean" = "gray")) +
  facet_grid(rows = vars(cityname), cols = vars(temp_type), scales = "free_y") +  
  labs(
    title = "Monthly Temperature Distributions",
    subtitle = "Restricted to the analyzed period May - September",
    x = "Month",
    y = "Temperature (°C)",
    fill = "Temperature Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size=12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.2, hjust=0.3),
    strip.text = element_text(size=12, face="bold")
  ) 

plot
}

# --- SAVE
# ggsave(
#   filename = "Monthly_Temperatures_City.png",
#   plot = plot,
#   width = 8, 
#   height = 12,
#   bg = "white",
#   dpi = 300
# )


# ------------------------------------------------------------------------------
#                              MORTALITY DATA
# ------------------------------------------------------------------------------

mort <- data.frame(date = ha_data$date,
                   mortality = ha_data$death,
                   cityname = ha_data$cityname
)

# Include this part if you want mortality per 100 000 inhabitants
# Attributes of demo:
# cityname, year, population.total
# Population count for each city for each year of the analyzed period
# Takes a bit longer to process

demo <- read.csv("Pl_demodata.csv")

for (city in cities){
  for (yr in years){
    val <- mort$mortality[mort$cityname == city & format(mort$date, "%Y") == yr]
    div <- demo$population.total[demo$cityname == city & demo$year == yr]
    mort$mortality[mort$cityname == city & format(mort$date, "%Y") == yr] <- val/div * 100000
  }
}

# -------

mort$month <- as.factor(month(mort$date))

{
  plot <- ggplot(mort, aes(x = date, y = mortality)) +
    geom_line(linewidth = 0.3, alpha = 1, color = "gray40") +
    labs(
      title = "City-specific Daily Mortality",
      x = NULL,
      y = "Number of All-Cause Deaths per 100 000 inhabitants"
    ) +
    scale_x_date(date_breaks = "4 year", 
                 date_labels = "%Y",
                 limits = as.Date(c('1991-01-01','2020-12-31')),
                 expand = c(0,0)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.text = element_text(size=12),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.2, hjust=0.3),
      strip.text = element_text(size=16, face="bold", hjust = 0)
    ) +
    facet_wrap(. ~ cityname, ncol = 1, scales = "free_y")
  
  
  plot
}

# --- SAVE
# ggsave(
#   filename = "Deaths_All.png",
#   plot = plot,
#   width = 8, 
#   height = 12,
#   dpi = 300,
#   bg = "white"
# )

# ------------------------------------------------------------------------------

# Distributions of Daily mortality by city

{
plot <- ggplot(mort, aes(x = cityname, y = mortality, fill = cityname)) +
  geom_boxplot(outlier.shape = NA, alpha = 1, fill = "gray") +

  labs(
    title = "Distribution of Daily Mortality by City",
    x = NULL,
    y = "All-Cause Deaths per 100 000 Inhabitants"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size=12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.2, hjust=0.3)
  )


plot
}

# --- SAVE
# ggsave(
#   filename = "Daily_Mortality_City.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white"
# )


# ------------------------------------------------------------------------------

# Monthly mortality by City
# Overview of seasonal patterns

{
  plot <- ggplot(mort, aes(x = month, y = mortality, fill = month)) +
    geom_boxplot(outlier.shape = NA, alpha = 1, fill = "gray") +
    labs(
      title = "Distribution of Monthly Mortality by City",
      x = NULL,
      y = "All-Cause Deaths per 100 000 Inhabitants"
    ) +
    ylim(1, 6.1) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.text = element_text(size=12),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.2, hjust=0.3),
      strip.text = element_text(size=16, face="bold", hjust = 0)
    ) +
    facet_wrap(. ~ cityname, ncol = 1, scales = "free_y")
  
  
  plot
}

# --- SAVE
# ggsave(
#   filename = "Monthly_Mortality_City.png",
#   plot = plot,
#   width = 8, 
#   height = 12,
#   bg = "white",
#   dpi = 300
# )

