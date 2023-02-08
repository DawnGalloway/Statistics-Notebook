curve(dt(x, 3), from=-4, to=4, lwd=2)
curve(dnorm(x), add=TRUE, col="gray")
abline(h=0, v=c(-1,1), col=c("gray","orange","orange"), lwd=c(1,2,2))

pt(-1, 3)*2 


cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)

pt(-2.601, 48)*2



round(pt(-1.285, 13)*2,4)

round(pt(-2.991, 48)*2,4)



cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)
confint(cars.lm)



