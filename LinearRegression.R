#Using tidyverse package

library(tidyverse)

#Assigns my own csv file to engines variable

engines <- read.csv(file = "Engines.csv")

twr <- list()
for (i in 1:18) {
  twr[i] <- engines$Thrust[i] / engines$Weight[i]
}
print(twr)

engines[["TWR"]] <- twr

print(engines$TWR)
print(engines)

#Creating basic linear model of Thrust to Weight

twr_model <- lm(Thrust ~ Weight, data = engines)
summary(model)

# Plotting model

twr_slope <- twr_model$coefficients[2]
print(twr_slope)

twr_plot <- ggplot(data = engines,
                   aes(x = Weight,
                       y = Thrust,
                       col = as.factor(Fuel)))

twr_plot +
  geom_point(size = 5) +
  geom_abline(slope = twr_slope, intercept = twr_model$coefficients[1]) +
  annotate("text", x = 1, y = 450,
           label = gettextf("Slope: %f", twr_model$coefficients[2]),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 1, y = 425,
           label = gettextf("R2: %f", summary(twr_model)$adj.r.squared),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 1, y = 475,
           label = gettextf("Intercept: %f", twr_model$coefficients[1]),
           size = 7, color = "SteelBlue")

# Second model comparing TWR and vac efficiency

eff_model <- lm(Ispvac ~ Group, engines)

summary(eff_model)

eff_intercept <- eff_model$coefficients[1]
eff_slope <- eff_model$coefficients[2]

print(eff_slope)

eff_plot <- ggplot(engines,
                   aes(x = Fuel,
                       y = Ispvac,
                       col = as.factor(Cycle),
                       label = Engine,
                       size = 1))

eff_plot +
  geom_point(size = 5) +
  geom_abline(slope = eff_slope, intercept = eff_intercept) +
  annotate("text", x = 1.5, y = 340,
           label = gettextf("Slope: %f", eff_model$coefficients[2]),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 1.5, y = 330,
           label = gettextf("R2: %f", summary(eff_model)$adj.r.squared),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 1.5, y = 350,
           label = gettextf("Intercept: %f", eff_model$coefficients[1]),
           size = 7, color = "SteelBlue") +
  geom_text(hjust = 0, vjust = 0)

# Final model, showing bad linear regression example

t_eff_model <- lm(Ispvac ~ unlist(TWR), engines)
summary(t_eff_model)

t_eff_intercept <- t_eff_model$coefficients[1]
t_eff_slope <- t_eff_model$coefficients[2]

t_eff_plot <- ggplot(engines,
                     aes(x = unlist(TWR),
                         y = Ispvac,
                         col = as.factor(Fuel)))

t_eff_plot +
  geom_point(size = 5) +
  geom_abline(slope = t_eff_slope, intercept = t_eff_intercept) +
  annotate("text", x = 150, y = 440,
           label = gettextf("Slope: %f", t_eff_model$coefficients[2]),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 150, y = 430,
           label = gettextf("R2: %f", summary(t_eff_model)$adj.r.squared),
           size = 7, color = "SteelBlue") +
  annotate("text", x = 150, y = 450,
           label = gettextf("Intercept: %f", t_eff_model$coefficients[1]),
           size = 7, color = "SteelBlue")