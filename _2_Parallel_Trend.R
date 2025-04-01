# ==============================================================================
# DID Analysis: Performing a Parallel Trends Check
# The results are purely informative
#
#
# Author: Mahulena Koristkova
# Last Update: 01-04-2025
# ==============================================================================

data.Trend <- read.csv("Eligible_All.csv")
implementation_year <- 2009

data.Trend$date <- as.Date(data.Trend$date)


cities <- unique(data.Trend$cityname)
years <- unique(data.Trend$year)

data.filtered <- data.Trend %>%
  filter(year < implementation_year)

# Omitting years with no mortality data (Poland)
data.filtered <- subset(data.filtered, (year != 1997) & (year != 1998))

data.filtered$X <- seq(1, nrow(data.filtered))

# Check NA vals
colSums(is.na(data.filtered))

# Use the same seasonal pattern as in the main model
dfseas <- 4
dftrend <- 1

# ==============================================================================
#                               Parallel Trends
#
# The loop will print all coefficients for all cities, look for interaction
# terms with low p-values
# ==============================================================================

# -------------------- A. BASIC Prediction



for (i in seq(cities)){
  citydat <- subset(data.filtered, cityname == cities[i])

  model <- glm(death ~ basic.predict +
                 basic.predict * ns(date, df = round(length(unique(year)) / dftrend / 10)) 
               + basic.predict * (ns(yday, df = dfseas) * factor(year)) 
               + basic.predict * as.factor(dow) 
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + ns(yday, df = dfseas) * factor(year)
               + as.factor(dow), 
               family = quasipoisson, 
               data = citydat)
  
 print(cities[i])
 print(summary(model))
}

# -------------------- A. SYNTHETIC Prediction

for (i in seq(cities)){
  citydat <- subset(data.filtered, cityname == cities[i])
  
  model <- glm(death ~ synth.predict +
                 synth.predict * ns(date, df = round(length(unique(year)) / dftrend / 10)) 
               + synth.predict * (ns(yday, df = dfseas) * factor(year)) 
               + synth.predict * as.factor(dow) 
               + ns(date, df = round(length(unique(year)) / dftrend / 10))
               + ns(yday, df = dfseas) * factor(year)
               + as.factor(dow), 
               family = quasipoisson, 
               data = citydat)
  
  print(cities[i])
  print(summary(model))
}


