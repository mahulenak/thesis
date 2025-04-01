# ==============================================================================
# DID Analysis: Data Preparation before running the models
#               
#
# Authors: Veronika Huber, Hanna Feldbusch, Mahulena Koristkova
# Last Update: 01-04-2025
# ==============================================================================

library(dplyr)
library(tidyr)

# =======================================
# DATA PREPARATION FOR DID APPROACH
# =======================================


data.raw <- read.csv("Eligible_All.csv")
implementation_year <- 2009

# Transform "no"/"yes" to 0/1

{
data.raw$date <- as.Date(data.raw$date)
cities <- unique(data.raw$cityname)

data.raw <- data.raw %>%
  mutate(hw = ifelse(is.na(hw), "no", "yes"),
         basic.predict = ifelse(basic.predict == "no", 0, 1),
         synth.predict = ifelse(synth.predict == "no", 0, 1),
         threshold.predict = ifelse(threshold.predict == "no", 0, 1))
}

# ==============================================================================
#                        PREPARATION FOR MAIN MODEL
# ==============================================================================


# NEW VARIABLE IMPLEMENTATION (before/ after HHWS Implementation)
data.raw$implementation <- ifelse(data.raw$year>=implementation_year, 1, 0)

# Exclude years with no mortality data (Poland)
data.raw <- subset(data.raw, (year != 1997) & (year != 1998))

# Main dataset
data.dlm <- data.raw

# Prepare datasets for sensitivity analyses
data.dlm.excl <- subset(data.dlm, (year < 2003) & (year > 2009))

data.dlm.excl.2 <- subset(data.dlm, (year != 1994))

data.dlm.excl.3 <- subset(data.dlm, (year != 2015))

# CHECK SUBSET FOR NAs
colSums(is.na(data.dlm))


