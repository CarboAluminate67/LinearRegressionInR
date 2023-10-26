library(tidyverse) #Using tidyverse package
library(ggplot2) #Using ggplot

#Assigns my own csv file to engines variable
engines <- read.csv(file = "Engines.csv")

#Creating basic linear model of Thrust to Weight
model <- lm(Thrust ~ Weight, data = engines)
summary(model)

# Plotting model
plot(engines$Weight, engines$Thrust, pch = 16,
     main = "Thrust To Weight", xlab = "Weight",
     ylab = "Thrust")

slope <- model$coefficients[2]
print(slope)

text(1500, 500, "Slope: model$coefficients[0]")
abline(model)
print(model$coefficients)
