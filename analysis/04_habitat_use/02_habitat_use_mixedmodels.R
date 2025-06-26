
# ------------------------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles
# Vertical habitat use Mixed models

# by Javier Menéndez-Blázquez | @jmenblaz



# ----------------------------------------------------------
# Analysed dives data derived from L3 ttdr files and dcalib (diveMove) (tracking/05_process_dives.R)
# for temporal autocorrelation
# LLM - tests
library(nlme)
library(car)
library(MuMIn)

# For GLMM and splines (final models)
library(glmmTMB) 
library(splines)
# library(splines2)

library(ggeffects)

# 3.1) Mixed Model for Day/night time

# Read previously exported data
data <- read.csv(paste0(input_dir,"/tracking/dives/dives_metrics.csv"))

# As factor
data$organismID <- as.factor(data$organismID)
data$season <- as.factor(data$season)
data$daynight <- as.factor(data$daynight)
data$moon_bright_class <- as.factor(data$moon_bright_class)

# format date
data$begdesc <- as.POSIXct(data$begdesc, format = "%Y-%m-%d %H:%M:%S")
data$endasc <- as.POSIXct(data$endasc, format = "%Y-%m-%d %H:%M:%S")

# day number of the year
data$num_day <- as.numeric(format(data$begdesc, "%j"))


# order data
data <- data[order(data$organismID, data$begdesc), ]


# check Normality ---
hist(data$meandep)
hist(data$maxdep)

# Kolmogorov-Smirnov (n > 5000)
ks.test(data$meandep, "pnorm", mean(data$meandep, na.rm=TRUE), sd(data$meandep, na.rm=TRUE))
ks.test(data$maxdep, "pnorm", mean(data$maxdep, na.rm=TRUE), sd(data$maxdep, na.rm=TRUE))

# No normality for both response variables

# Log transformed
data$log_meandep <- log(data$meandep)
data$log_maxdep <- log(data$maxdep)

hist(data$log_meandep)
hist(data$log_maxdep)

ks.test(data$log_meandep, "pnorm",
         mean(data$log_meandep, na.rm=TRUE),
         sd(data$log_meandep, na.rm=TRUE))

ks.test(data$log_maxdep, "pnorm",
        mean(data$log_maxdep, na.rm=TRUE),
        sd(data$log_maxdep, na.rm=TRUE))

# qqnorm(data$log_meandep); qqline(data$log_meandep, col = "red")
# qqnorm(data$log_maxdep); qqline(data$log_maxdep, col = "red")




# Testing GLMM Gaussian distribution with log transformation and 
# splines for autocorrelation

# Subset
#data <- data[sample(nrow(data), 5000), ]


# Model df 8 for meandep response variables (supplementary material reduced models results comparation)
model <- glmmTMB(
  meandep ~ daynight + season + splines::ns(num_day, df = 8) + (1 | organismID),
  data = data,
  family = Gamma(link = "log")
)

summary(model)

# plot(ggpredict(model, terms = c("daynight", "season")))
# plot(ggpredict(model, terms = "num_day [all]"))

# Model df 5 for maxndep response variables (supplementary material reduced models results comparation)
# Modelo con spline sobre el día del año
model <- glmmTMB(
  maxdep ~ daynight + season + splines::ns(num_day, df = 8) + (1 | organismID),
  data = data,
  family = Gamma(link = "log")
)

summary(model)

plot(ggpredict(model, terms = c("daynight", "season")))
plot(ggpredict(model, terms = "num_day [all]"))




