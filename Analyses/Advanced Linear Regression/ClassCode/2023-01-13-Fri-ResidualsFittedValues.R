library(tidyverse)
View(diamonds)
plot(price ~ carat, data=diamonds)

diamonds.lm <- lm(price ~ carat, data=diamonds)
summary(diamonds.lm)

abline(diamonds.lm, col="green", lwd=2)





plot(dist ~ speed, data=cars)
cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)
abline(cars.lm)
points(dist ~ speed, data=cars[23,], col="orange", pch=16)

View(cars)
cars[23, ]
cars.lm$residuals[23]
cars$dist[23] - cars.lm$fitted.values[23] #Y_i - Yhat_i
