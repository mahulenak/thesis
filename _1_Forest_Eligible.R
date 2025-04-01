# ==============================================================================
# DID Analysis: Assigning Pre-Implementation Eligibility for 
#               Heat Alerts, Forest Model Optimization + Training
#
# Data Upsampling: 1) "Basic" Replication (V. Huber + H. Feldbusch)
#                  2) Applying SMOTE Algorithm (K-nearest neighbors)
#                     (M. Koristkova)
#
# Hyperparameter optimization: choosing optimal maxnodes, nodesize, mtry
# (M. Koristkova)
#
# Forest Model Training + Prediction on Pre-Implementation period 
# (V. Huber + H. Feldbusch)
#
# Authors: Veronika Huber, Hanna Feldbusch, Mahulena Koristkova
# Last Update: 01-04-2025
# ==============================================================================

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lattice)
library(caret)
library(randomForest)
library(zoo)
library(pdp)
# library(imbalance)
library(patchwork)
#library(ggpattern) 
library(gridExtra)
#library(xtable)
library(viridis)
library(smotefamily)

# Dataset Attributes:
# date (YYYY-MM-DD), year, month, day, yday (day of year), dow (day of week),
# death, tmean, tmax, tmin, cityname, D2tmean, D2tmax, D2tmin, D3tmean, D3tmax,
# D3tmin (2- and 3-day rolling means), hw (hw == 1 for real heat alert days,
# NA otherwise)

data.merged <- read.csv("HA_Meteo_Death.csv")
implementation_year <- 2009  # HEWS implementation

# ------------------------------------------------------------------------------

cities <- unique(data.merged$cityname)
set.seed(123)

# ------------------------------------------------------------------------------
#                   Preparation for Up-Sampling + Model Training
# ------------------------------------------------------------------------------

# Restrict to Post-Implementation Period and Warm Months
data.randomForest <- subset(data.merged, year >= implementation_year)
data.randomForest <- subset(data.randomForest, month %in% c(5, 6, 7, 8, 9))
data.randomForest$month <- factor(data.randomForest$month)

# For ha == NA, assign "no", otherwise assign "yes"
data.randomForest <- data.randomForest %>%
  mutate(hw = ifelse(is.na(hw), "no", "yes"))

# Find days with heat alerts with very low recorded temperatures + omit them
# Looking for outliers within the maximum temperatures (tmax)
# 1) Identify OUTLIERS 
# 2) Clean Them Based on the Interquartile Criterion

# Looking for outliers within the positive class
data.outliers <- subset(data.randomForest, hw == "yes")

# Find + Extract Outliers
out <- boxplot.stats(data.outliers$tmax)$out
out_ind_tmax <- which(data.outliers$tmax %in% c(out))

# Save them to a table, for reporting purposes
outliers.table <- data.outliers[out_ind_tmax, ]

# What tmax corresponds to the lower bound of the IQR?
tmax_threshold <- as.numeric(quantile(data.outliers$tmax, 0.25) - 
                               1.5 * IQR(data.outliers$tmax))

# Reassign the hw class on days where tmax <= IQR
data.randomForest <- data.randomForest %>%
  mutate(hw = ifelse(hw == "yes" & tmax <= tmax_threshold, "no", hw))


rm(out, out_ind_tmax, data.outliers, tmax_threshold, outliers.table)
# ------------------------------------------------------------------------------

# Check NA values within the dataset:
colSums(is.na(data.randomForest))

# How many heat alert days are there?
nrow(subset(data.randomForest, hw == "yes")) 

# ------------------------------------------------------------------------------
#
#                Up-Sampling + Model Optimization + Model Trainig
#
# ------------------------------------------------------------------------------

data.randomForest$hw <- factor(data.randomForest$hw)

# ------------------------------------------------------------------------------
#                      1. REPLICATION: "BASIC" Up-Sampling
# ------------------------------------------------------------------------------

{
# CREATE NEW DATASETS WITH SAMPLED INDICES
Basic.Yes <- which(data.randomForest$hw == "yes")
Basic.No <- which(data.randomForest$hw == "no")

# OVER-SAMPLING BY INCREASING THE SIZE OF THE CLASS "yes"
Basic.Yes.Up <- sample(Basic.Yes, length(Basic.No), replace=T)
Basic.Up <- data.randomForest[c(Basic.Yes.Up, Basic.No),]

rm(Basic.Yes, Basic.No, Basic.Yes.Up)
}

# SPLITTING DATA IN TRAINING AND TEST DATASET
{
  ind.up <- sample(2, nrow(Basic.Up), replace=T, prob=c(0.7,0.3)) 
  Train.Basic <- Basic.Up[ind.up==1,] # TRAINING
  Test.Basic <- Basic.Up[ind.up==2,] # TESTING
  
  rm(ind.up)
}

# ------------------------------------------------------------------------------
#                          TRAINING THE FOREST MODEL
# ------------------------------------------------------------------------------

# --------------------------------------------------------
# Cross - Validation
# Looking for optimal parameters of maxnodes and nodesize
# 
# Uncomment for Parameter Tuning
# Default mtry setting: sqrt(number of variables)
# Look for mtry close to this value
# --------------------------------------------------------


# UNCOMMENT FOR HYPERPARAMETER TUNING:
# This takes a long time   

# tune.grid <- expand.grid(
#   mtry = c(3, 4, 5)
# )
# 
# # Train with cross-validation
# # number == n-fold cross-validation
# # caret package
# train.control <- trainControl(method = "cv", number = 10)  # 10-fold CV
# 
# ns_catalog <- c(1, 10, 20, 30, 50)
# mn_catalog <- c(5, 10, 50, 100, 200, 500, 1000, 2000, 5000, 7500, 10000)
# 
# {
# mtry_opt <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                          dimnames = list(ns_catalog, mn_catalog))
# 
# mtry_opt <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                    dimnames = list(ns_catalog, mn_catalog))
# 
# CV_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                  dimnames = list(ns_catalog, mn_catalog))
# 
# train_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                        dimnames = list(ns_catalog, mn_catalog))
# 
# test_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                       dimnames = list(ns_catalog, mn_catalog))
# 
# p_vals <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                    dimnames = list(ns_catalog, mn_catalog))
# }
# 
# for (i in seq_along(ns_catalog)) {
#   for (j in seq_along(mn_catalog)) {
# 
#     rf_model <- train(
#       hw ~ tmean + tmax + tmin + D2tmean +
#         D2tmax + D2tmin + D3tmean + D3tmax + D3tmin,
#       data = Train.Basic,
#       method = "rf",
#       tuneGrid = tune.grid,
#       trControl = train.control,
#       nodesize = ns_catalog[i],
#       maxnodes = mn_catalog[j]
#     )
# 
#     # Optimal mtry
#     mtry_opt[i, j] <- rf_model$bestTune$mtry
# 
#     # CV Accuracy
#     acc <- max(rf_model$results$Accuracy)  # Get highest CV - accuracy
#     CV_acc[i, j] <- acc
# 
#     # Test accuracy
#     test_preds <- predict(rf_model, newdata = Test.Basic)
#     test_acc[i, j] <- mean(test_preds == Test.Basic$hw)
# 
#     # Train accuracy
#     train_preds <- predict(rf_model, Train.Basic)
#     train_acc[i, j] <- mean(train_preds == Train.Basic$hw)
# 
#     # P-Value
#     orig_pred <- predict(rf_model, data.randomForest)
#     cfm_model <- confusionMatrix(orig_pred, data.randomForest$hw)
#     p_vals[i, j] <- cfm_model$overall["AccuracyPValue"]
#     gc()
#   }
# }
# 
# # Arrange the results into one data frame:
# 
# CV.Basic <- expand.grid(
#   nodesize = ns_catalog,
#   maxnodes = mn_catalog,
#   KEEP.OUT.ATTRS = FALSE
# )
# 
# # Add corresponding values from matrices
# {
# CV.Basic$CV.Acc   <- as.vector(CV_acc)
# CV.Basic$Train.Acc <- as.vector(train_acc)
# CV.Basic$Test.Acc <- as.vector(test_acc)
# CV.Basic$mtry     <- as.vector(mtry_opt)
# CV.Basic$p.val    <- as.vector(p_vals)
# }
# 
# # Save as csv
# write.csv(CV.Basic, "CV_Basic.csv", row.names = FALSE)

# # -----------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                               MODEL TRAINING
# ------------------------------------------------------------------------------

# Optimal parameters selected based on tuning performance:
{
mtry.basic <- 5
mn.basic <- 500
ns.basic <- 30
}

data.Basic <- data.randomForest

RF.Basic <- randomForest(hw ~ tmean + tmax + tmin + D2tmean + 
                              D2tmax + D2tmin + D3tmean + D3tmax + D3tmin,
                         data=Train.Basic,
                         importance = TRUE,
                         ntree = 500,
                         mtry=mtry.basic,
                         maxnodes=mn.basic, 
                         nodesize=ns.basic
                         )

# MAKE PREDICTIONS
{
Train.Basic$predict <- predict(RF.Basic, Train.Basic)
Test.Basic$predict <- predict(RF.Basic, Test.Basic)
data.Basic$predict <- predict(RF.Basic, data.Basic)
}

# CALCULATE CUNFUSION MATRICES & QUALITY CRITERIA
{
# CFM: Train dataset (Upsampled!)
CFM.Basic.Train <- confusionMatrix(Train.Basic$predict, Train.Basic$hw, 
                                   dnn=c("Prediction","Reference"))
print("CFM - Basic, Train")
print(CFM.Basic.Train)

# CFM: Test dataset (Upsampled!)
CFM.Basic.Test <- confusionMatrix(Test.Basic$predict, Test.Basic$hw, 
                                  dnn=c("Prediction","Reference"))
print("CFM - Basic, Test")
print(CFM.Basic.Test)

# CFM: Original dataset (No upsampling)
CFM.Basic.Orig <- confusionMatrix(data.Basic$predict, data.Basic$hw, 
                                  dnn=c("Prediction","Reference"))
print("CFM - Basic, Orig")
print(CFM.Basic.Orig)
}

rm(Test.Basic, Train.Basic, mtry.basic, mn.basic, ns.basic)

# ------------------------------------------------------------------------------
#                  2. SYNTHETIC UPSAMPLING - SMOTE ALGORITHM
# ------------------------------------------------------------------------------

# Choose variable which should be up-sampled:
data.SMOTE <- subset(data.randomForest, select = 
                       c("tmean", "tmax", "tmin", "D2tmean", "D2tmax", 
                         "D2tmin", "D3tmean", "D3tmax", "D3tmin", "hw"))

data.SMOTE.in <- subset(data.SMOTE, select = c("tmean", "tmax", "tmin", "D2tmean", "D2tmax", 
                                               "D2tmin", "D3tmean", "D3tmax", "D3tmin"))
# Choose the predictor variable:
data.SMOTE.target <- subset(data.SMOTE, select = c("hw"))

# K-nearest neighbors, select K:
up.SMOTE <- SMOTE(data.SMOTE.in, data.SMOTE.target, K = 5)

# Extract the Up-Sampled dataset
Synth.Up <- up.SMOTE$data

# Rename predictor variable back to hw
Synth.Up <- Synth.Up %>% 
  rename(hw = class)

Synth.Up$hw <- as.factor(Synth.Up$hw)


# SPLITTING DATA IN TRAINING AND TEST DATASET
{
  sample_index <- sample(1:nrow(Synth.Up), size = 0.7 * nrow(Synth.Up)) # 70% training
  Train.Synth <- Synth.Up[sample_index, ]  
  Test.Synth <- Synth.Up[-sample_index, ] 
  rm(sample_index, Synth.Up)
}


# ------------------------------------------------------------------------------
#                          TRAINING THE FOREST MODEL
# ------------------------------------------------------------------------------

# --------------------------------------------------------
# Cross - Validation
# Looking for optimal parameters of maxnodes and nodesize
# 
# Uncomment for Parameter Tuning
# Default mtry setting: sqrt(number of variables)
# Look for mtry close to this value
# --------------------------------------------------------


# UNCOMMENT FOR HYPERPARAMETER TUNING:

# 
# tune.grid <- expand.grid(
#   mtry = c(3, 4, 5)
# )
# 
# # Train with cross-validation
# # number == n-fold cross-validation
# # caret package
# train.control <- trainControl(method = "cv", number = 10)  # 10-fold CV
# 
# ns_catalog <- c(1, 10, 20, 30, 50)
# mn_catalog <- c(5, 10, 50, 100, 200, 500, 1000, 2000, 5000, 7500, 10000)
# 
# {
#   mtry_opt <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                      dimnames = list(ns_catalog, mn_catalog)) 
#   
#   mtry_opt <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                      dimnames = list(ns_catalog, mn_catalog))
#   
#   CV_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                    dimnames = list(ns_catalog, mn_catalog))
#   
#   train_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                       dimnames = list(ns_catalog, mn_catalog))
#   
#   test_acc <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                      dimnames = list(ns_catalog, mn_catalog))
#   
#   p_vals_synth <- matrix(NA, nrow = length(ns_catalog), ncol = length(mn_catalog),
#                    dimnames = list(ns_catalog, mn_catalog))
# }
# 
# for (i in seq_along(ns_catalog)) {
#   for (j in seq_along(mn_catalog)) {
#     
#     rf_model <- train(
#       hw ~ tmean + tmax + tmin + D2tmean + 
#         D2tmax + D2tmin + D3tmean + D3tmax + D3tmin, 
#       data = Train.Synth, 
#       method = "rf",
#       tuneGrid = tune.grid,
#       trControl = train.control,
#       nodesize = ns_catalog[i],
#       maxnodes = mn_catalog[j]
#     )
#     
#     # Optimal mtry
#     mtry_opt[i, j] <- rf_model$bestTune$mtry
#     
#     # CV Accuracy
#     acc <- max(rf_model$results$Accuracy)  # Get highest CV - accuracy
#     CV_acc[i, j] <- acc
#     
#     # Test accuracy
#     test_preds <- predict(rf_model, newdata = Test.Synth)
#     test_acc[i, j] <- mean(test_preds == Test.Synth$hw)
#     
#     # Train accuracy
#     train_preds <- predict(rf_model, Train.Synth)
#     train_acc[i, j] <- mean(train_preds == Train.Synth$hw)
#     
#     # P-Value
#     orig_pred <- predict(rf_model, data.randomForest)
#     cfm_model <- confusionMatrix(orig_pred, data.randomForest$hw)
#     p_vals_synth[i, j] <- cfm_model$overall["AccuracyPValue"]
#   }
# }
# 
# rm(CV_acc, mtry_opt, test_acc, train_acc, rf_model, tune.grid, train.control,
#    i, j, ns_catalog, mn_catalog, test_preds, train_preds, 
#    acc, p_vals, p_vals_synth, cfm_model, orig_pred)
# 
# # Arrange the results into one data frame:
# 
# CV.Synth <- expand.grid(
#   nodesize = ns_catalog,
#   maxnodes = mn_catalog,
#   KEEP.OUT.ATTRS = FALSE
# )
# 
# # Add corresponding values from matrices
# {
# CV.Synth$CV.Acc   <- as.vector(CV_acc)
# CV.Synth$Train.Acc <- as.vector(train_acc)
# CV.Synth$Test.Acc <- as.vector(test_acc)
# CV.Synth$mtry     <- as.vector(mtry_opt)
# CV.Synth$p.val    <- as.vector(p_vals_synth)
# }
# 
# # Save as csv
# write.csv(CV.Synth, "CV_Synth.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
#                               MODEL TRAINING
# ------------------------------------------------------------------------------

data.Synth <- data.randomForest

{
  mtry.synth <- 5
  mn.synth <- 500
  ns.synth <- 30
}

RF.Synth <- randomForest(hw ~ tmean + tmax + tmin + D2tmean + D2tmax + 
                           D2tmin + D3tmean + D3tmax + D3tmin,
                         data=Train.Synth,
                         importance = TRUE,
                         ntree = 500,
                         mtry=mtry.synth,
                         maxnodes=mn.synth, 
                         nodesize=ns.synth)

# MAKE A PREDICTION ON TRAINING DATASET
{
Train.Synth$predict <- predict(RF.Synth, Train.Synth)
Test.Synth$predict <- predict(RF.Synth, Test.Synth)
data.Synth$predict <- predict(RF.Synth, data.Synth)
}

# CALCULATE CUNFUSION MATRICES & QUALITY CRITERIA
{
# CFM: Train dataset (Upsampled!)
CFM.Synth.Train <- confusionMatrix(Train.Synth$predict, Train.Synth$hw, 
                                   dnn=c("Prediction","Reference"))
print("CFM, Train Synth Up")
print(CFM.Synth.Train)

# CFM: Test dataset (Upsampled!)
CFM.Synth.Test <- confusionMatrix(Test.Synth$predict, Test.Synth$hw, 
                                  dnn=c("Prediction","Reference"))

print("CFM, Test Synth Up")
print(CFM.Synth.Test)

# CFM: Original dataset (No upsampling)
CFM.Synth.Orig <- confusionMatrix(data.Synth$predict, data.Synth$hw, 
                                  dnn=c("Prediction","Reference"))

print("CFM, Synth Orig")
print(CFM.Synth.Orig)
}

rm(Train.Synth, Test.Synth, mtry.synth, ns.synth, mn.synth)

rm(CFM.Basic.Orig, CFM.Basic.Test, CFM.Basic.Train,
   CFM.Synth.Orig, CFM.Synth.Test, CFM.Synth.Train)


# ----------------------------------------------------------------------------
#
#             P E R F O R M A N C E    O N    K N O W N    D A T A
# 
# ----------------------------------------------------------------------------

# Combine Predictions of Both Models:

{
data.Performance <- data.randomForest
data.Performance$basic.predict <- data.Basic$predict
data.Performance$synth.predict <- data.Synth$predict
rm(data.Synth, data.Basic)
}

# Basic model False Positives + False Negatives
FP.Basic <- subset(data.Performance, (basic.predict == "yes") & (hw == "no"))
FN.Basic <- subset(data.Performance, (basic.predict == "no") & (hw == "yes"))

# Synth model False Positives + False Negatives
FP.Synth <- subset(data.Performance, (synth.predict == "yes") & (hw == "no"))
FN.Synth <- subset(data.Performance, (synth.predict == "no") & (hw == "yes"))

# Print FP/FN To Console
for (i in seq(cities)){
  city <- cities[i]
  city.fp.b <- subset(FP.Basic, cityname == city)
  city.fp.s <- subset(FP.Synth, cityname == city)
  city.fn.s <- subset(FN.Synth, cityname == city)
  city.fn.b <- subset(FN.Basic, cityname == city)
  city.match.b <- subset(data.Performance, cityname == city & hw == "yes" & basic.predict == "yes")
  city.match.s <- subset(data.Performance, cityname == city & hw == "yes" & synth.predict == "yes")

  print(city)
  print(paste("FP.B = ", nrow(city.fp.b), "FP.S = ", nrow(city.fp.s),
              "FN.S = ", nrow(city.fn.s), "FN.B = ", nrow(city.fn.b)))
  print(paste("Basic Match = ", nrow(city.match.b), "Synth Match = ", nrow(city.match.s)))
}

rm(city, city.fp.b, city.fp.s, city.fn.s, city.match.b, city.match.s, i)

# Print numbers of Real Heat Alerts for all cities to Console
for (i in seq(cities)){
  city <- cities[i]
  print(city)
  print(nrow(subset(data.Performance, hw == "yes" & cityname == city)))
}

rm(i, city)
rm(FN.Basic, FN.Synth, FP.Basic, FP.Synth)


# # ----------------------------------------------------------------------------
# #               C R I T E R I U M - B A S E D    A P P R O A C H
# # How well does applying Heat Alert criteria to Meteo Data reproduce heat alerts?
# # Code is tailored to Polish data and HEWS
# # ----------------------------------------------------------------------------
# {
# data.crit <- subset(data.merged,year>=implementation_year)
# data.crit <- subset(data.crit, month %in% c(5, 6, 7, 8, 9))
# data.crit$date <- as.Date(data.crit$date)
# data.crit$hw.crit <- FALSE
# }
# 
# # CRITERIA:
# # A 
# # 24. 4. 2008 - 13. 5. 2009
# # Tmax > 30 For 2+ Days
# # Tmax >= 35 For 1+ Days
# 
# # B, C, D
# # 13. 5. 2009 - 18. 7. 2017
# # Tmax > 30 for 2+ Days
# 
# # E
# # 19. 7. 2017 - 5. 5. 2019
# # Tmax >= 30 For 2+ Days
# 
# # F
# # 6. 5. 2019 - curr.
# # Tmax >= 30 For 2+ Days
# # Tmax >= 35 For 1+ Days
# 
# 
# # Only HA is of interest, not its intensity. So we can only deal with 4 sub periods
# {
# start_A <- "2009-01-01"
# end_A <- "2009-05-12"
# start_BCD <- "2009-05-13"
# end_BCD <- "2017-07-18"
# start_E <- "2017-07-19"
# end_E <- "2019-05-05"
# start_F <- "2019-05-06"
# end_F <- "2020-12-31"
# }
# 
# 
# for (i in seq_along(cities)) {
# 
#   city <- subset(data.crit, cityname == cities[i])
#   
#   # Check for period A:
#   sub.A <- city[city$date >= as.Date(start_A) & city$date <= as.Date(end_A), ]
#   sub.A <- sub.A[order(sub.A$date), ]
#   
#   for (j in 2:nrow(sub.A)) {
#     if (sub.A$tmax[j] > 30 & sub.A$tmax[j - 1] > 30) {
#       sub.A$hw.crit[j] <- TRUE
#       sub.A$hw.crit[j - 1] <- TRUE
#     }
#     
#     if (sub.A$tmax[j] >= 35){
#       sub.A$hw.crit[j] <- TRUE
#     }
#   }
#   
#   # Check for period BCD:
#   sub.BCD <- city[city$date >= as.Date(start_BCD) & city$date <= as.Date(end_BCD), ]
#   sub.BCD <- sub.BCD[order(sub.BCD$date), ]
#   
#   for (j in 2:nrow(sub.BCD)) {
#     if (sub.BCD$tmax[j] > 30 & sub.BCD$tmax[j - 1] > 30) {
#       sub.BCD$hw.crit[j] <- TRUE
#       sub.BCD$hw.crit[j - 1] <- TRUE
#     }
#   }
#   
#   # Check for period E:
#   sub.E <- city[city$date >= as.Date(start_E) & city$date <= as.Date(end_E), ]
#   sub.E <- sub.E[order(sub.E$date), ]
#   
#   for (j in 2:nrow(sub.E)) {
#     if (sub.E$tmax[j] >= 30 & sub.E$tmax[j - 1] >= 30) {
#       sub.E$hw.crit[j] <- TRUE
#       sub.E$hw.crit[j - 1] <- TRUE
#     }
#   }
#   
#   
#   # Check for period F:
#   sub.F <- city[city$date >= as.Date(start_F) & city$date <= as.Date(end_F), ]
#   sub.F <- sub.F[order(sub.F$date), ]
#   
#   for (j in 2:nrow(sub.F)) {
#     if (sub.F$tmax[j] >= 30 & sub.F$tmax[j - 1] >= 30) {
#       sub.F$hw.crit[j] <- TRUE
#       sub.F$hw.crit[j - 1] <- TRUE
#     }
#     
#     if (sub.F$tmax[j] >= 35){
#       sub.F$hw.crit[j] <- TRUE
#     }
#     
#   }
#   
#   
#   # Merge dataframes
#   city.data <- rbind(sub.A, sub.BCD, sub.E, sub.F)
#   
#   assign(paste0("data_", cities[i]), city.data)
# 
# }
# 
# 
# data.crit <- rbind(data_Wroclaw, data_Warsaw, data_Poznan, data_Krakow, data_Lodz)
# 
# {
# rm(data_Wroclaw, data_Warsaw, data_Poznan, data_Krakow, data_Lodz)
# rm(start_A, start_BCD, start_E, start_F, end_A, end_BCD, end_E, end_F)
# rm(sub.A, sub.BCD, sub.E, sub.F, city, city.data, i, j)
# }
# 
# # Number of actual predictions:
# nrow(subset(data.crit, hw == 1))
# 
# # Nrow: 790
# data.crit.HA <- subset(data.crit, ((hw == 1) | (hw.crit == TRUE)))
# nrow(data.crit.HA)
# 
# # Number of matching predictions = 464
# crit.match <- subset(data.crit, ((hw == 1) & (hw.crit == TRUE)))
# nrow(crit.match)
# crit.fn <- subset(data.crit, ((hw == 1) & (hw.crit == FALSE)))
# nrow(crit.fn)
# crit.fp <- subset(data.crit, ((is.na(hw)) & (hw.crit == TRUE)))
# nrow(crit.fp)
# 
# for (i in seq(cities)){
#   city <- cities[i]
#   
#   print(city)
#   print(paste("FN = ", nrow(subset(crit.fn, cityname == city)), "FP = ", 
#               nrow(subset(crit.fp, cityname == city)), "Match = ",
#               nrow(subset(crit.match, cityname == city))))
#   
# }
# 
# rm(crit.match, crit.fn, crit.fp, city, i)
# 
# # ---------------------------------------------------------------
# # PLOT: Tmax on days when hw == 1, but not according to criteria
# # ---------------------------------------------------------------
# # FALSE NEGATIVES
# 
# df.plot <-subset(data.crit, (hw == 1) & (hw.crit == FALSE))
# 
# plot <- ggplot(df.plot, aes(x = seq(1, nrow(df.plot)))) +
#   geom_point(aes(y = tmax, fill = "tmax"), color = "red", alpha = 0.6) +
#   labs(x = NULL, 
#        y = "T(max) (°C)", 
#        fill = "") +
#   scale_fill_manual(values = c("tmax" = "blue"), 
#                     labels = c("T(max) - measured")) +
#   theme_minimal() +
#   geom_hline(yintercept = 30, linetype = "dashed", color = "black") +
#   theme(legend.position = "none",
#         legend.text = element_text(size=12),
#         panel.border = element_rect(color = "black", fill = NA),
#         plot.title = element_text(size = 16, face = "bold"),
#         plot.subtitle = element_text(size = 14),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x = element_blank()
#   )
# 
# plot
# 
# ggsave(
#   filename = "FN_Crit.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white",
#   dpi = 300
# )
# 
# rm(df.plot)
# # ---------------------------------------------------------------------------------
# # PLOT: Tmax on days when hw == 0, but according to criteria there should have been
# # ---------------------------------------------------------------------------------
# 
# df.plot <-subset(data.crit, ((is.na(hw)) & (hw.crit == TRUE)))
# 
# plot <- ggplot(df.plot, aes(x = seq(1, nrow(df.plot)))) +
#   geom_point(aes(y = tmax, fill = "tmax"), color = "red", alpha = 0.6) +
#   labs(x = NULL, 
#        y = "T(max) (°C)", 
#        fill = "") +
#   scale_fill_manual(values = c("tmax" = "blue"), 
#                     labels = c("T(max) - measured")) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         legend.text = element_text(size=12),
#         panel.border = element_rect(color = "black", fill = NA),
#         plot.title = element_text(size = 16, face = "bold"),
#         plot.subtitle = element_text(size = 14),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x = element_blank()
#   )
# 
# plot
# 
# ggsave(
#   filename = "FP_Crit.png",
#   plot = plot,
#   width = 7, 
#   height = 4,
#   bg = "white",
#   dpi = 300
# )
# 
# rm(df.plot, plot, data.crit, data.crit.HA)


# ----------------------------------------------------------------------------
#
#                F  I  N  A  L    P  R  E  D  I  C  T  I  O  N
#                           Pre-Implementation Data
#
# ----------------------------------------------------------------------------

{
  data.Final <- data.merged
  data.Final <- subset(data.Final, month %in% c(5, 6, 7, 8, 9))
  data.Final$month <- as.factor(data.Final$month)

  data.Final <- data.Final %>%
    mutate(hw = ifelse(hw == 1 & tmax <= 25.55, NA, hw))
  
  Pred.Basic <- predict(RF.Basic, data.Final)
  Pred.Synth <- predict(RF.Synth, data.Final)
  
  data.Final$basic.predict <- Pred.Basic
  data.Final$synth.predict <- Pred.Synth
  
  data.Final$basic.predict <- as.character(data.Final$basic.predict)
  data.Final$synth.predict <- as.character(data.Final$synth.predict)
}


data.Final <- data.Final %>%
  mutate(
    synth.predict = case_when(
      year >= 2009 & hw == 1 ~ "yes",
      year >= 2009 & is.na(hw) ~ "no",
      TRUE ~ synth.predict
    ),
    basic.predict = case_when(
      year >= 2009 & hw == 1 ~ "yes",
      year >= 2009 & is.na(hw) ~ "no",
      TRUE ~ basic.predict
    )
  )

# Add threshold prediction
# Set the thresholds according to the HEWS

data.Final$threshold.predict <- "no"

for (i in seq(cities)){
  citydat <- subset(data.Final, cityname == cities[i])
  citydat <- citydat[order(citydat$date), ]
  
  for (j in 2:nrow(citydat)) {
    if (citydat$tmax[j] >= 30 & citydat$tmax[j - 1] >= 30) {
      citydat$threshold.predict[j] <- "yes"
      citydat$threshold.predict[j - 1] <- "no"
    }
  }
  
  assign(paste0("data_", cities[i]), citydat)
  
}

# Replace with citynames from your dataset
data.Final <- rbind(data_Wroclaw, data_Warsaw, data_Poznan, data_Krakow, data_Lodz)
rm(data_Wroclaw, data_Warsaw, data_Poznan, data_Krakow, data_Lodz)

# Save
write.csv(data.Final, "Eligible_All.csv", row.names = FALSE)

rm(data.Performance, data.Unknown, Unknown.Pred, Pred.Basic, Pred.Synth)
rm(col_order, Pred.Synth, data.Synth, data.merged, data.randomForest)
rm(RF.Basic, RF.Synth)






