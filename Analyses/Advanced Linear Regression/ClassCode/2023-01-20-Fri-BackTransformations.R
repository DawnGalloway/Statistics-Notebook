lm.log <- lm(log(circumference) ~ age, data=Orange)
b.log <- coef(lm.log)
b.log[1]
b.log[2]

lm.sqrt <- lm(sqrt(circumference) ~ age, data=Orange)
b.sqrt <- coef(lm.sqrt)

lm.1oY <- lm(1/circumference ~ age, data=Orange)
b.1oY <- coef(lm.1oY)

lm.y <- lm(circumference ~ age, data=Orange)
b.y <- coef(lm.y)

lm.y2 <- lm(circumference^2 ~ age, data=Orange)
b.y2 <- coef(lm.y2)

lm.1oY2 <- lm(1/circumference^2 ~ age, data=Orange)
b.1oY2 <- coef(lm.1oY2)


# Base Graphic


plot(circumference ~ age, data=Orange, pch=16, col="orangered", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")

curve( exp(b.log[1] + b.log[2]*x), add=TRUE, col="red")
curve( (b.sqrt[1] + b.sqrt[2]*x)^2, add=TRUE, col="blue")
curve( 1/(b.1oY[1] + b.1oY[2]*x), add=TRUE, col="green")
curve( b.y[1] + b.y[2]*x, add=TRUE, col="gray")
curve( sqrt(b.y2[1] + b.y2[2]*x), add=TRUE, col="orange")
curve( 1/sqrt(b.1oY2[1] + b.1oY2[2]*x), add=TRUE, col="forestgreen")

# ggplot Graphic

library(tidyverse)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x), aes(color="log(Y)")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2, aes(color="sqrt(Y)")) + 
  stat_function(fun=function(x) 1/(b.1oY[1] + b.1oY[2]*x), aes(color="1/Y")) + 
  stat_function(fun=function(x) b.y[1] + b.y[2]*x, aes(color="Y")) + 
  stat_function(fun=function(x) sqrt(b.y2[1] + b.y2[2]*x), aes(color="Y^2")) + 
  stat_function(fun=function(x) 1/sqrt(b.1oY2[1] + b.1oY2[2]*x), aes(color="1/Y^2")) + 
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( ) 

library(car)
boxCox(lm.y)


YoungOrange <- filter(Orange, age < 1200)


lm.log <- lm(log(circumference) ~ age, data=YoungOrange)
b.log <- coef(lm.log)

lm.sqrt <- lm(sqrt(circumference) ~ age, data=YoungOrange)
b.sqrt <- coef(lm.sqrt)

lm.1oY <- lm(1/circumference ~ age, data=YoungOrange)
b.1oY <- coef(lm.1oY)

lm.y <- lm(circumference ~ age, data=YoungOrange)
b.y <- coef(lm.y)

lm.y2 <- lm(circumference^2 ~ age, data=YoungOrange)
b.y2 <- coef(lm.y2)

lm.1oY2 <- lm(1/circumference^2 ~ age, data=YoungOrange)
b.1oY2 <- coef(lm.1oY2)



ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  geom_vline(aes(xintercept=1200)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x), aes(color="log(Y)")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2, aes(color="sqrt(Y)")) + 
#  stat_function(fun=function(x) 1/(b.1oY[1] + b.1oY[2]*x), aes(color="1/Y")) + 
  stat_function(fun=function(x) b.y[1] + b.y[2]*x, aes(color="Y")) + 
  stat_function(fun=function(x) sqrt(b.y2[1] + b.y2[2]*x), aes(color="Y^2")) + 
#  stat_function(fun=function(x) 1/sqrt(b.1oY2[1] + b.1oY2[2]*x), aes(color="1/Y^2")) + 
  ylim(c(0,300)) + 
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( ) 
