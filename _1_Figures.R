# ====================================
# Up-Sampling + Forest model figures
# Run after Forest_Eligible
# Author: Mahulena Koristkova
# Last Update: 01-04-2025
# ====================================


# ------------------------------------------------------------------------------
#                        Up-Sampling: DATASET COMPARISON
# Creates a 2x2 grid with:
# 1/ Original Dataset
# 2/ Dataset with Removed Outliers
# 3/ Dataset Up-Sampled using the "Basic" approach
# 4/ Dataset Up-Sampled using the "Synthetic" approach
# ------------------------------------------------------------------------------
data.orig <- subset(data.merged,year >= implementation_year)
data.orig <- subset(data.orig, month %in% c(5, 6, 7, 8, 9))
data.orig <- data.orig %>%
  mutate(hw = ifelse(is.na(hw), "no", "yes"))
data.orig <- subset(data.orig, hw == "yes")

{
  alph_set <- 0.1
  col_set <- "#21908CFF"
  
  # Original Dataset
  plot1 <- ggplot(data.orig, aes(x = tmean, y = tmax)) +
    geom_point(size = 3, alpha = alph_set, color = col_set) +
    theme_minimal() +
    labs(
      title = "Original dataset",
      x = NULL,
      y = "Max Temp. (°C)"
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 12, face = "bold"), 
      legend.position = "none"
    )
  
  # Reduced outliers
  plot2 <- ggplot(up.SMOTE$orig_P %>% filter(class == "yes"), aes(x = tmean, y = tmax)) +
    geom_point(size = 3, alpha = alph_set, color = col_set) +
    theme_minimal() +
    labs(
      title = "Outliers removed",
      x = NULL,
      y = NULL
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 12, face = "bold"), 
      legend.position = "none"
    )
  
  # Basic Upsampling
  plot3 <- ggplot(Basic.Up %>% filter(hw == "yes"), aes(x = tmean, y = tmax)) +
    geom_point(size = 3, alpha = alph_set, color = col_set) +
    theme_minimal() +
    labs(
      title = "Basic up-sampling",
      x = "Mean Temp. (°C)",
      y = "Max Temp. (°C)"
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 12, face = "bold"), 
      legend.position = "none"
    )
  
  # Synth Upsampling
  plot4 <- ggplot(up.SMOTE$data %>% filter(class == "yes"), aes(x = tmean, y = tmax)) +
    geom_point(size = 3, alpha = alph_set, color = col_set) +
    theme_minimal() +
    labs(
      title = "Synthetic up-sampling",
      x = "Mean Temp. (°C)",
      y = NULL
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 12, face = "bold"), 
      legend.position = "none"
    )
  
  
  # Combine plots with shared legend
  combined_plot <- (plot1 + plot2 + plot3 + plot4) + 
    plot_layout(ncol = 2, nrow = 2, guides = "collect") &
    theme(legend.position = "none", 
          panel.border = element_rect(color = "black", fill = NA),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))

  combined_plot
}


# ggsave(
#   filename = "Dataset_Comparison.png",  
#   plot = combined_plot,          
#   width = 7,                   
#   height = 7,                   
#   dpi = 300                     
# )

rm(plot1, plot2, plot3, plot4, data.orig, combined_plot, alph_set, col_set, 
   data.plot)


# ------------------------------------------------------------------------------
#              VARIABLE IMPORTANCE PLOTS (nicer than the default ones)
#                  Mean Decrease Gini + Mean Decrease Accuracy
# ------------------------------------------------------------------------------

# 1. BASIC RANDOM FOREST MODEL

# Extract variable Importance
var_importance <- as.data.frame(importance(RF.Basic))
var_importance$Variable <- rownames(var_importance)

# Plot for Mean Decrease Accuracy
p1 <- ggplot(var_importance, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                                 y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "grey90", width = 0.7, color = "grey10") +
  coord_flip() +
  labs(title = "Mean Decrease Accuracy",
       x = NULL, 
       y = "Importance Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Plot for Mean Decrease Gini
p2 <- ggplot(var_importance, aes(x = reorder(Variable, MeanDecreaseGini), 
                                 y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "grey90", width = 0.7, color = "grey10") +
  coord_flip() +
  labs(title = "Mean Decrease Gini",
       x = NULL, 
       y = "Importance Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

final_plot <- p1 + p2 + plot_layout(ncol = 2)

print(final_plot)

# ggsave(
#   filename = "VIP_Basic.png",  
#   plot = final_plot,          
#   width = 7,                   
#   height = 4,                   
#   dpi = 300                     
# )

# 2. SYNTHETIC RANDOM FOREST MODEL

var_importance <- as.data.frame(importance(RF.Synth))
var_importance$Variable <- rownames(var_importance)

# Plot for Mean Decrease Accuracy
p1 <- ggplot(var_importance, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                                 y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "grey90", width = 0.7, color = "grey10") +
  coord_flip() +
  labs(title = "Mean Decrease Accuracy",
       x = NULL, 
       y = "Importance Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Plot for Mean Decrease Gini
p2 <- ggplot(var_importance, aes(x = reorder(Variable, MeanDecreaseGini), 
                                 y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "grey90", width = 0.7, color = "grey10") +
  coord_flip() +
  labs(title = "Mean Decrease Gini",
       x = NULL, 
       y = "Importance Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

final_plot <- p1 + p2 + plot_layout(ncol = 2)

print(final_plot)

# ggsave(
#   filename = "VIP_Synth.png",  
#   plot = final_plot,          
#   width = 7,                   
#   height = 4,                   
#   dpi = 300                     
# )


rm(p1, p2, final_plot, var_importance)

# ------------------------------------------------------------------------------
#                         ERROR RATES + Out of Bag Errors
#                         These plots take longer to show
# ------------------------------------------------------------------------------

# 1. SYNTHETIC MODEL

oob_data <- as.data.frame(RF.Synth$err.rate)
oob_data$Trees <- 1:nrow(oob_data) # Add tree numbers
oob_long <- pivot_longer(oob_data, cols = -Trees, names_to = "Error_Type", 
                         values_to = "Error_Rate")
rm(oob_data)

custom_colors <- c("yes" = "grey10", "no" = "grey10", "OOB" = "grey10")  
custom_labels <- c("yes" = "Eligible", "no" = "Non-Eligible", "OOB" = "OOB")
custom_linetypes <- c("yes" = "solid", "no" = "dotted", "OOB" = "dashed")  


# Create the OOB error plot
plot <- ggplot(oob_long, aes(x = Trees, y = Error_Rate, linetype = Error_Type)) +
  geom_line(linewidth = 1, color = "grey10") +  # Add lines for each error type
  scale_linetype_manual(values = custom_linetypes) +  # Custom line types
  labs(
    x = "Number of Trees",
    y = "Error Rate",
    color = NULL
  ) +
  ylim(0, 0.063) +
  geom_label(aes(x = 400, y = 0.062, label = "No"), size = 6) +
  geom_label(aes(x = 400, y = 0.025, label = "Yes"), size = 6)+
  geom_label(aes(x = 400, y = 0.043, label = "OOB"), size = 6) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot

# --- SAVE
# ggsave(
#   filename = "OOB_Synthetic.png",
#   plot = plot,
#   width = 6, 
#   height = 3.5,
#   bg = "white"
# )

rm(plot, oob_long)


# 2. BASIC MODEL

oob_data <- as.data.frame(RF.Basic$err.rate)
oob_data$Trees <- 1:nrow(oob_data) # Add tree numbers
oob_long <- pivot_longer(oob_data, cols = -Trees, names_to = "Error_Type", 
                         values_to = "Error_Rate")
rm(oob_data)

custom_linetypes <- c("yes" = "solid", "no" = "dotted", "OOB" = "dashed")  

# Create the OOB error plot
plot <- ggplot(oob_long, aes(x = Trees, y = Error_Rate, linetype = Error_Type)) +
  geom_line(linewidth = 1, color = "grey10") +  # Add lines for each error type
  scale_linetype_manual(values = custom_linetypes) +  # Custom line types
  labs(
    x = "Number of Trees",
    y = "Error Rate",
    color = NULL
  ) +
  geom_label(aes(x = 400, y = 0.06, label = "No"), size = 6) +
  geom_label(aes(x = 400, y = 0.01, label = "Yes"), size = 6)+
  geom_label(aes(x = 400, y = 0.033, label = "OOB"), size = 6) +
  ylim(0, 0.063) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot

# --- Save
# ggsave(
#   filename = "OOB_Basic.png",
#   plot = plot,
#   width = 6, 
#   height = 3.5,
#   bg = "white"
# )

rm(plot, oob_long, custom_linetypes, custom_colors, custom_labels)

# ------------------------------------------------------------------------------
#                HEATMAPS: VISUALISATION OF HYPERPARAMETER TUNING 
# ------------------------------------------------------------------------------

# 1. BASIC MODEL

CV.Basic <- read.csv("CV_Basic.csv", sep="\t")

heatmap.Test <- ggplot(CV.Basic, aes(x = factor(maxnodes), y = factor(nodesize), 
                                     fill = Test.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(Test.Acc, 3), 
                color = ifelse(Test.Acc > median(Test.Acc), "black", "white")), 
            size = 5) +
  scale_color_identity() +  # Use direct color values
  theme_minimal() +
  labs(
    title = "Test Accuracy",
    x = NULL,
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

heatmap.Train <- ggplot(CV.Basic, aes(x = factor(maxnodes), y = factor(nodesize), 
                                      fill = Train.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(Train.Acc, 3), 
                color = ifelse(Train.Acc > median(Train.Acc), "black", "white")),  
            size = 5) +
  scale_color_identity() +  # Use direct color values
  theme_minimal() +
  labs(
    title = "Train Accuracy",
    x = NULL,
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )


heatmap.CV <- ggplot(CV.Basic, aes(x = factor(maxnodes), y = factor(nodesize), 
                                   fill = CV.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(CV.Acc, 3), 
                color = ifelse(CV.Acc > median(CV.Acc), "black", "white")),  
            size = 5) +
  scale_color_identity() +  
  theme_minimal() +
  labs(
    title = "CV Accuracy",
    x = "Max Nodes",
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )


combined_plot <- (heatmap.Train + heatmap.Test + heatmap.CV) + 
  plot_layout(ncol = 1, nrow=3, guides = "collect") &
  scale_fill_viridis_c(option = "viridis", limits = c(0.925, 1)) &
  theme(legend.position = "none", 
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


combined_plot

# ggsave(
#   filename = "Basic_Heatmaps.png",
#   plot = combined_plot,
#   width = 7, 
#   height = 11,
#   bg = "white",
#   dpi = 300
# )

# 2. SYNTHETIC MODEL

CV.Synth <- read.csv("CV_Synth.csv", sep="\t")

heatmap.Test <- ggplot(CV.Synth, aes(x = factor(maxnodes), y = factor(nodesize), fill = Test.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(Test.Acc, 3), 
                color = ifelse(Test.Acc > median(Test.Acc), "black", "white")),  # Invert logic for viridis
            size = 5) +
  scale_color_identity() +  # Use direct color values
  theme_minimal() +
  labs(
    title = "Test Accuracy",
    x = NULL,
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

heatmap.Train <- ggplot(CV.Synth, aes(x = factor(maxnodes), y = factor(nodesize), fill = Train.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(Train.Acc, 3), 
                color = ifelse(Train.Acc > median(Train.Acc), "black", "white")),  # Invert logic for viridis
            size = 5) +
  scale_color_identity() +  # Use direct color values
  theme_minimal() +
  labs(
    title = "Train Accuracy",
    x = NULL,
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )


heatmap.CV <- ggplot(CV.Synth, aes(x = factor(maxnodes), y = factor(nodesize), fill = CV.Acc)) +
  geom_tile(color = "white") +  # White grid lines
  geom_text(aes(label = round(CV.Acc, 3), 
                color = ifelse(CV.Acc > median(CV.Acc), "black", "white")),  # Invert logic for viridis
            size = 5) +
  scale_color_identity() +  
  theme_minimal() +
  labs(
    title = "CV Accuracy",
    x = "Max Nodes",
    y = "Node Size",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )


combined_plot <- (heatmap.Train + heatmap.Test + heatmap.CV) + 
  plot_layout(ncol = 1, nrow=3, guides = "collect") &
  scale_fill_viridis_c(option = "viridis", limits = c(0.925, 1)) &
  theme(legend.position = "none", 
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


combined_plot

# ggsave(
#   filename = "Synth_Heatmaps.png",
#   plot = combined_plot,
#   width = 7, 
#   height = 11,
#   bg = "white",
#   dpi = 300
# )

rm(heatmap.CV, heatmap.Test, heatmap.Train, CV.Basic, CV.Synth, combined_plot)



# ------------------------------------------------------------------------------
#                                   FP/FN PLOTS
# ------------------------------------------------------------------------------

# -----------------------------------
# False Positives: Basic Up-Sampling
# -----------------------------------

FP_basic <- ggplot(FP.Basic, aes(x = seq(1, nrow(FP.Basic)))) +
  geom_point(aes(y = tmean, fill = "tmean"), color = "black", alpha = 0.6) +
  geom_point(aes(y = tmax, fill = "tmax"), color = "red", alpha = 0.6) +
  geom_point(aes(y = tmin, fill = "tmin"), color = "blue", alpha = 0.6) +
  geom_hline(yintercept = mean(FP.Basic$tmax), color = "red") +
  geom_hline(yintercept = mean(FP.Basic$tmean), color = "black") +
  geom_hline(yintercept = mean(FP.Basic$tmin), color = "blue") +
  labs(x = NULL, 
       y = "Temperature (°C)", 
       fill = "",
       title = "False Positives, Basic up-sampling") +
  scale_fill_manual(values = c("tmax" = "red", 
                               "tmean" = "black", 
                               "tmin" = "blue"), 
                    labels = c(paste("T(max); mean = ", round(mean(FP.Basic$tmax), 2)), 
                               paste("T(mean); mean = ", round(mean(FP.Basic$tmean), 2)),
                               paste("T(min); mean = ", round(mean(FP.Basic$tmin), 2)))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()
  )

FP_basic

# ggsave(
#   filename = "FP_Basic.png",
#   plot = FP_basic,
#   width = 7, 
#   height = 4,
#   bg = "white",
#   dpi = 300
# )

rm(FP_basic)

# -----------------------------------
# False Positives: Synth. Up-Sampling
# -----------------------------------

plot <- ggplot(FP.Synth, aes(x = seq(1, nrow(FP.Synth)))) +
  geom_point(aes(y = tmean, fill = "tmean"), color = "black", alpha = 0.6) +
  geom_point(aes(y = tmax, fill = "tmax"), color = "red", alpha = 0.6) +
  geom_point(aes(y = tmin, fill = "tmin"), color = "blue", alpha = 0.6) +
  geom_hline(yintercept = mean(FP.Synth$tmax), color = "red") +
  geom_hline(yintercept = mean(FP.Synth$tmean), color = "black") +
  geom_hline(yintercept = mean(FP.Synth$tmin), color = "blue") +
  labs(x = NULL, 
       y = "Temperature (°C)", 
       fill = "",
       title = "False Positives, Synthetic up-sampling") +
  scale_fill_manual(values = c("tmax" = "red", 
                               "tmean" = "black", 
                               "tmin" = "blue"), 
                    labels = c(paste("T(max); mean = ", round(mean(FP.Synth$tmax), 2)), 
                               paste("T(mean); mean = ", round(mean(FP.Synth$tmean), 2)),
                               paste("T(min); mean = ", round(mean(FP.Synth$tmin), 2)))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()
  )

# ggsave(
#   filename = "FP_Synth.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white",
#   dpi = 300
# )

# -----------------------------------
# False Negatives: Synth. Up-Sampling
# -----------------------------------

plot <- ggplot(FN.Synth, aes(x = seq(1, nrow(FN.Synth)))) +
  geom_point(aes(y = tmean, fill = "tmean"), color = "black", alpha = 0.6) +
  geom_point(aes(y = tmax, fill = "tmax"), color = "red", alpha = 0.6) +
  geom_point(aes(y = tmin, fill = "tmin"), color = "blue", alpha = 0.6) +
  geom_hline(yintercept = mean(FN.Synth$tmax), color = "red") +
  geom_hline(yintercept = mean(FN.Synth$tmean), color = "black") +
  geom_hline(yintercept = mean(FN.Synth$tmin), color = "blue") +
  labs(x = NULL, 
       y = "Temperature (°C)", 
       fill = "",
       title = "False Negatives, Synthetic up-sampling") +
  scale_fill_manual(values = c("tmax" = "blue", 
                               "tmean" = "red", 
                               "tmin" = "green"), 
                    labels = c(paste("T(max); mean = ", round(mean(FN.Synth$tmax), 2)), 
                               paste("T(mean); mean = ", round(mean(FN.Synth$tmean), 2)),
                               paste("T(min); mean = ", round(mean(FN.Synth$tmin), 2)))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()
  )

# ggsave(
#   filename = "FN_Synth.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white",
#   dpi = 300
# )

rm(plot)

# ! Include False Negative Plot for Basic up-sampling, if you have any

# ------------------------------------------------------------------------------
#                     Mortality on Heat Alert Days
# ------------------------------------------------------------------------------

# 1. Post-Implementation Period

# Filter the dataset for years starting with the implementation year

filtered_data <- data.Final %>%
  filter(year >= implementation_year & year <= 2020 & hw == 1)

# Create boxplots faceted by city
ggplot(filtered_data, aes(x = factor(year), y = death)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  facet_wrap(~ cityname, scales = "free_y", ncol = 1,) +  # One plot per city
  labs(
    x = "Year",
    y = "Deaths",
    title = "Yearly Distribution of Deaths (2009–2020) by City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )

# 2. Pre-Implementation Period

filtered_data <- data.Final %>%
  filter(year < implementation_year & synth.predict == "yes")

# Create boxplots faceted by city
ggplot(filtered_data, aes(x = factor(year), y = death)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  facet_wrap(~ cityname, scales = "free_y", ncol = 1,) +  # One plot per city
  labs(
    x = "Year",
    y = "Deaths",
    title = "Yearly Distribution of Deaths (2009–2020) by City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )

# ------------------------------------------------------------------------------
#               Mortality on Heat Alert Days + The 3 Following Days
#                     Compared to Mortality on Non-Alert Days
# ------------------------------------------------------------------------------


# Step 1: Tag "Post-synth" exposure window
data.flagged <- data.Final %>%
  arrange(cityname, date) %>%
  group_by(cityname) %>%
  mutate(
    exposure_window = as.integer(synth.predict == "yes")  # Start by marking exposure days
  ) %>%
  mutate(
    exposure_window = exposure_window + 
      lead(exposure_window, 1, default = 0) +
      lead(exposure_window, 2, default = 0) +
      lead(exposure_window, 3, default = 0),
    exposed = case_when(
      exposure_window > 0 ~ "Eligible + 3",
      synth.predict == "no" ~ "Non-Eligible",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(year < implementation_year, !is.na(exposed))

# Step 2: Plot side-by-side boxplots by year and exposure
plot <- ggplot(data.flagged, aes(x = factor(year), y = death, fill = exposed)) +
  geom_boxplot(outlier.size = 0.8, position = position_dodge(width = 0.8)) +
  facet_grid(cityname ~ ., scales = "free_y") +
  scale_fill_manual(values = c("Eligible + 3" = "tomato", "Non-Eligible" = "gray90")) +
  labs(
    x = NULL,
    y = "Deaths",
    fill = NULL,
    title = "Deaths on Eligible days + 3 days after vs. Deaths on Non-Eligible days",
    subtitle = "Synthetic Up-Sampling"
  ) +
  theme_minimal() +
theme(
  legend.position = "bottom",
  legend.text = element_text(size = 12),
  panel.border = element_rect(color = "black", fill = NA),
  plot.title = element_text(size = 16, face = "bold"),
  plot.subtitle = element_text(size = 14),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 12),
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(angle = 0, hjust = 0),
  strip.text = element_text(size = 12, face="bold"),
  strip.placement = "outside"
)

plot

# ggsave(
#   filename = "Deaths_Dist_Synth.png",
#   plot = plot,
#   width = 8, 
#   height = 12,
#   bg = "white",
#   dpi = 300
# )


