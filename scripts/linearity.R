# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Data Input -------------------------------------------------------------
nh4.data.all <- read_excel("H:/GitHub Projects/Ammonia_Validation/data/Ammonia by HACH Validation Workbook.xlsx", 
                           sheet = "Raw Data")

# Data Cleaning ----------------------------------------------------------

nh4.data.all$Level <- as.numeric(nh4.data.all$Level)
nh4.data.all <- nh4.data.all[,-9]  # remove phantom column

# Low Linearity ----------------------------------------------------------

nh4.low <- nh4.data.all %>%
  filter(Test == "Linearity" & Type == "Low")

# Low curve fit ----------------------------------------------------------
low.fit <- lm(Reading~Level, data = nh4.low)
summary(low.fit)
confint(low.fit)

# Visualising Low --------------------------------------------------------

low.plot <- ggplot(low.fit, aes(x=Level, y = Reading)) +
  geom_point(size=4, shape=21, col = "grey25", fill = "cornflowerblue") +
  geom_smooth(method=lm) +
  geom_abline(slope=1, intercept=0, lty=2, col="red") +
  labs(title = "Low Standard Curve", y = "Absorbance", x = "Ammonia, mg/L")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))

low.plot

# High Linearity ----------------------------------------------------------

nh4.high <- nh4.data.all %>%
  filter(Test == "Linearity" & Type == "High")

# Low curve fit ----------------------------------------------------------
high.fit <- lm(Reading~Level, data = nh4.high)
summary(high.fit)
confint(high.fit)

# Visualising High --------------------------------------------------------

high.plot <- ggplot(high.fit, aes(x=Level, y = Reading)) +
  geom_point(size=4, shape=21, col = "grey25", fill = "cornflowerblue") +
  geom_smooth(method=lm) +
  geom_abline(slope=1, intercept=0, lty=2, col="red") +
  labs(title = "High Standard Curve", y = "Absorbance", x = "Ammonia, mg/L")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))

high.plot

# Sensitivity ------------------------------------------------------------

low_sens <- nh4.low$Reading[42]/nh4.low$Level[42]
high_sens <- nh4.high$Reading[42]/nh4.high$Level[42]
