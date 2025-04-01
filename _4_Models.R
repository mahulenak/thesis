# ==============================================================================
# DIFFERENCE-IN-DIFFERENCES 
# Quasi - Poisson Regression using DLM
# + Forest Plots
#
# AUTHORS: Hanna Feldbusch & Veronika Huber, Mahulena Koristkova
# Last update: 01-04-2025
# ==============================================================================


library(splines)
library(dlnm)
library(reshape2)
#library(lmtest)
library(tseries) 
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------
#    MODEL SPECIFICATIONS 
# ---------------------------

# Input Dataset
data.sm <- data.dlm


{
# SPECIFICATION OF THE LAG FUNCTION
lag <- 3
# DEGREE OF FREEDOM FOR SEASONALITY
dfseas <- 4

# DEGREE OF FREEDOM FOR TREND
dftrend <- 1
}

# # FUNCTION FOR COMPUTING THE Q-AIC
# QAIC <- function(model) {
#   phi <- summary(model)$dispersion
#   loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
#   return(-2*loglik + 2*summary(model)$df[3]*phi)
# }



# -----------------------------------
#       PREPARE EMPTY CONTAINERS
# -----------------------------------

{

modelvar <- c("model0","model1","model2", "model3", "model4", "model5")

# ARRAY TO STORE RR FOR ALERT AND ALERT*IMPLEMENTATION BY CITY AND MODEL SPECIFICATION
RRalert <- RRalertint <- array(NA,dim=c(length(cities),length(modelvar),3),
                               dimnames=list(cities,modelvar,c("est","ci.l","ci.u")))
}


# ------------------------------------------------------------------------------
#                        QUASI-POISSON MODELS FOR EACH CITY
# ------------------------------------------------------------------------------

for (i in seq(cities)) {
  
  tryCatch({
  
  city <- cities[i]
  print(city)
  
  # CREATE A SUBSET FOR EACH CITY
  citydat <- subset(data.sm, cityname %in% city)
  
# ------------------------------------------------------------------------------
#                            ELIGIBILITY: SYNTHETIC
# ------------------------------------------------------------------------------

  cbalert0 <- crossbasis(citydat$synth.predict, lag=lag,
                        argvar=list(fun="lin"),
                        arglag=list(fun="integer"),
                        group=citydat$year)
  
  alertint0 <- citydat$implementation*cbalert0

  model0 <- glm(death ~ cbalert0 + implementation + alertint0
                 + ns(yday, df = dfseas):factor(year)
                 + ns(date, df = round(length(unique(year)) / dftrend / 10))
                 + as.factor(dow),
                 family = quasipoisson,
                 data = citydat)

  
# ------------------------------------------------------------------------------
#                            ELIGIBILITY: BASIC
# ------------------------------------------------------------------------------

  cbalert1 <- crossbasis(citydat$basic.predict, lag=lag,
                       argvar=list(fun="lin"),
                       arglag=list(fun="integer"),
                       group=citydat$year)
 
  alertint1 <- citydat$implementation*cbalert1
 
  model1 <- glm(death ~ cbalert1 + implementation + alertint1
               + ns(yday, df = dfseas):factor(year)
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + as.factor(dow),
               family = quasipoisson,
               data = citydat)
 
# ------------------------------------------------------------------------------
#                            ELIGIBILITY: THRESHOLD
# ------------------------------------------------------------------------------

 cbalert2 <- crossbasis(citydat$threshold.predict, lag=lag,
                       argvar=list(fun="lin"),
                       arglag=list(fun="integer"),
                       group=citydat$year)
 
 alertint2 <- citydat$implementation*cbalert2
 
 model2 <- glm(death ~ cbalert2 + implementation + alertint2
               + ns(yday, df = dfseas):factor(year)
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + as.factor(dow),
               family = quasipoisson,
               data = citydat)
 
# ------------------------------------------------------------------------------
#                     ELIGIBILITY: EXCLUDE 2003 - 2009
# ------------------------------------------------------------------------------
 citydat3 <- subset(data.dlm.excl.1, cityname %in% city)

 cbalert3 <- crossbasis(citydat3$synth.predict, lag=lag,
                       argvar=list(fun="lin"),
                       arglag=list(fun="integer"),
                       group=citydat3$year)
 
 alertint3 <- citydat3$implementation*cbalert3
 
 model3 <- glm(death ~ cbalert3 + implementation + alertint3
               + ns(yday, df = dfseas):factor(year)
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + as.factor(dow),
               family = quasipoisson,
               data = citydat3)
 
# ------------------------------------------------------------------------------
#                        ELIGIBILITY: EXCLUDE 1994
# ------------------------------------------------------------------------------
 
 citydat4 <- subset(data.dlm.excl.2, cityname %in% city)
 
 cbalert4 <- crossbasis(citydat4$synth.predict, lag=lag,
                        argvar=list(fun="lin"),
                        arglag=list(fun="integer"),
                        group=citydat4$year)
 
 alertint4 <- citydat4$implementation*cbalert4
 
 model4 <- glm(death ~ cbalert4 + implementation + alertint4
               + ns(yday, df = dfseas):factor(year)
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + as.factor(dow),
               family = quasipoisson,
               data = citydat4)

# ------------------------------------------------------------------------------
#                      ELIGIBILITY: EXCLUDE 2015
# ------------------------------------------------------------------------------
 
 citydat5 <- subset(data.dlm.excl.3, cityname %in% city)
 
 cbalert5 <- crossbasis(citydat5$synth.predict, lag=lag,
                        argvar=list(fun="lin"),
                        arglag=list(fun="integer"),
                        group=citydat5$year)
 
 alertint5 <- citydat5$implementation*cbalert5
 
 model5 <- glm(death ~ cbalert5 + implementation + alertint5
               + ns(yday, df = dfseas):factor(year)
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + as.factor(dow),
               family = quasipoisson,
               data = citydat5)
 
 # ----------------------------------------------------------------------------
  
  # FOR ALERT EFFECT
  # SAVE CUMULATIVE RR 
 
  palert0 <- crossreduce(cbalert0, model0, at=1)
  RRalert[i,"model0","est"] <- palert0$RRfit
  RRalert[i,"model0","ci.l"] <- palert0$RRlow
  RRalert[i,"model0","ci.u"] <- palert0$RRhigh
  
  palert1 <- crossreduce(cbalert1, model1,at=1)
  RRalert[i,"model1","est"] <- palert1$RRfit
  RRalert[i,"model1","ci.l"] <- palert1$RRlow
  RRalert[i,"model1","ci.u"] <- palert1$RRhigh
  
  palert2 <- crossreduce(cbalert2,model2,at=1)
  RRalert[i,"model2","est"] <- palert2$RRfit
  RRalert[i,"model2","ci.l"] <- palert2$RRlow
  RRalert[i,"model2","ci.u"] <- palert2$RRhigh
  
  palert3 <- crossreduce(cbalert3,model3,at=1)
  RRalert[i,"model3","est"] <- palert3$RRfit
  RRalert[i,"model3","ci.l"] <- palert3$RRlow
  RRalert[i,"model3","ci.u"] <- palert3$RRhigh
  
  palert4 <- crossreduce(cbalert4,model4,at=1)
  RRalert[i,"model4","est"] <- palert4$RRfit
  RRalert[i,"model4","ci.l"] <- palert4$RRlow
  RRalert[i,"model4","ci.u"] <- palert4$RRhigh
  
  palert5 <- crossreduce(cbalert5,model5,at=1)
  RRalert[i,"model5","est"] <- palert5$RRfit
  RRalert[i,"model5","ci.l"] <- palert5$RRlow
  RRalert[i,"model5","ci.u"] <- palert5$RRhigh
  
  
  # FOR INTERACTION ALERT*IMPLEMENTATION
  # SAVE CUMULATIVE RR AND REDUCED COEF AND VCOV
  
  palertint0 <- crossreduce(alertint0, model0, at=1)
  RRalertint[i,"model0","est"] <- palertint0$RRfit
  RRalertint[i,"model0","ci.l"] <- palertint0$RRlow
  RRalertint[i,"model0","ci.u"] <- palertint0$RRhigh
  print("Model 0")
  print(paste(round(palertint0$RRfit,3), round(palertint0$RRlow,3), round(palertint0$RRhigh,3)))
  
  palertint1 <- crossreduce(alertint1, model1, at=1)
  RRalertint[i,"model1","est"] <- palertint1$RRfit
  RRalertint[i,"model1","ci.l"] <- palertint1$RRlow
  RRalertint[i,"model1","ci.u"] <- palertint1$RRhigh
  
  print("Model 1")
  print(paste(round(palertint1$RRfit,3), round(palertint1$RRlow,3), round(palertint1$RRhigh,3)))

  palertint2 <- crossreduce(alertint2, model2, at=1)
  RRalertint[i,"model2","est"] <- palertint2$RRfit
  RRalertint[i,"model2","ci.l"] <- palertint2$RRlow
  RRalertint[i,"model2","ci.u"] <- palertint2$RRhigh
  
  print("Model 2")
  print(paste(round(palertint2$RRfit,3), round(palertint2$RRlow,3), round(palertint2$RRhigh,3)))

  palertint3 <- crossreduce(alertint3, model3, at=1)
  RRalertint[i,"model3","est"] <- palertint3$RRfit
  RRalertint[i,"model3","ci.l"] <- palertint3$RRlow
  RRalertint[i,"model3","ci.u"] <- palertint3$RRhigh
  
  print("Model 3")
  print(paste(round(palertint3$RRfit,3), round(palertint3$RRlow,3), round(palertint3$RRhigh,3)))
  
  palertint4 <- crossreduce(alertint4, model4, at=1)
  RRalertint[i,"model4","est"] <- palertint4$RRfit
  RRalertint[i,"model4","ci.l"] <- palertint4$RRlow
  RRalertint[i,"model4","ci.u"] <- palertint4$RRhigh
  
  print("Model 4")
  print(paste(round(palertint4$RRfit,3), round(palertint4$RRlow,3), round(palertint4$RRhigh,3)))
  
  palertint5 <- crossreduce(alertint5, model5, at=1)
  RRalertint[i,"model5","est"] <- palertint5$RRfit
  RRalertint[i,"model5","ci.l"] <- palertint5$RRlow
  RRalertint[i,"model5","ci.u"] <- palertint5$RRhigh
  
  print("Model 5")
  print(paste(round(palertint5$RRfit,3), round(palertint5$RRlow,3), round(palertint5$RRhigh,3)))

  assign(paste0(cities[i],"_model0"), summary(model0)$coefficients)
  assign(paste0(cities[i],"_model1"), summary(model1)$coefficients)
  assign(paste0(cities[i],"_model2"), summary(model2)$coefficients)
  assign(paste0(cities[i],"_model3"), summary(model3)$coefficients)
  assign(paste0(cities[i],"_model4"), summary(model4)$coefficients)
  assign(paste0(cities[i],"_model5"), summary(model5)$coefficients)
  
  gc()
  print("done")
  
  }, error = function(e) {
    print(paste("Error processing", city, ":", e))
  })
}


# ------------------------------------------
# FOREST PLOT RR FOR HEAT ALERT EFFECT
# ------------------------------------------


# RRalert is a 3D array with dimensions: cities x models x (est, ci.l, ci.u)
# Convert RRalert to a data frame
RRalert_df <- as.data.frame.table(RRalert, responseName = "value")
colnames(RRalert_df) <- c("city", "model", "type", "value")

# Reshape data to wide format for est, ci.l, and ci.u
RRalert_wide <- RRalert_df %>%
  pivot_wider(names_from = type, values_from = value)

# Create the forest plot with customizations
{
forest_plot <- ggplot(RRalert_wide, aes(x = est, y = city)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  geom_errorbar(aes(xmin = ci.l, xmax = ci.u, color = model),
                position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.7) +
  geom_point(aes(color = model), position = position_dodge(width = 0.7), size = 3) +
  theme_minimal() +
  labs(
    title = paste("Relative Risk: Main Model"),
    x = "RR (heat alert effect)",
    y = NULL,  # Remove "City" y-axis label
    color = ""
  ) +
    scale_color_manual(
      values = c("#c02e1d", "#f16c20", "#a2b86c", "#1395ba", "#6c4ea2", "#e8c547"),  
      labels = c("Model 0 (Synth. + RD)", 
                 "Model 1 (Basic + RD)", 
                 "Model 2 (Threshold + RD)",
                 "Model 3 (Synth. + RD, Excl. 2003-2009)",
                 "Model 4 (Synth. + RD, Excl. 1994)",
                 "Model 5 (Synth. + RD, Excl. 2015)") 
  ) +
  guides(color = guide_legend(ncol = 2)) + # Number of columns in legend
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

forest_plot
  }

# 
# ggsave("RRalert.png", 
#        forest_plot, 
#        width = 7, 
#        height = 6, 
#       bg = "white")

# ------------------------------------------------------------------
# FORESTPLOT RR FOR HEAT ALERT * IMPLEMENTATION INTERACTION EFFECT
# ------------------------------------------------------------------
{
RRalert_df <- as.data.frame.table(RRalertint, responseName = "value")
colnames(RRalert_df) <- c("city", "model", "type", "value")

RRalert_wide <- RRalert_df %>%
  pivot_wider(names_from = type, values_from = value)

# Create the forest plot with customizations
forest_plot <- ggplot(RRalert_wide, aes(x = est, y = city)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  geom_errorbar(aes(xmin = ci.l, xmax = ci.u, color = model), 
                position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.7) +  # Add caps to error bars
  geom_point(aes(color = model), position = position_dodge(width = 0.7), size = 3) +  # Bigger points
  theme_minimal() +
  labs(
    title = paste0("Relative Risk: ", lag, "-Day Lag"),
    x = expression(paste(zeta, " (Alert \u00D7 Implementation)" )),
    y = NULL,  
    color = ""
  ) +
  xlim(0.5, 1.5)+
  scale_color_manual(
    values = c("#c02e1d", "#f16c20", "#a2b86c", "#1395ba", "#6c4ea2", "#e8c547"),  
    labels = c("Model 0 (Synth. + RD)", 
               "Model 1 (Basic + RD)", 
               "Model 2 (Threshold + RD)",
               "Model 3 (Synth. + RD, Excl. 2003-2009)",
               "Model 4 (Synth. + RD, Excl. 1994)",
               "Model 5 (Synth. + RD, Excl. 2015)") 
  ) +
  guides(color = guide_legend(ncol = 2)) +  
  theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )

forest_plot

}

# ggsave(paste0("RRalertINT_lag",lag, ".png"), 
#        forest_plot, 
#        width = 7.1, 
#        height = 9,
#        bg = "white"
#        )


