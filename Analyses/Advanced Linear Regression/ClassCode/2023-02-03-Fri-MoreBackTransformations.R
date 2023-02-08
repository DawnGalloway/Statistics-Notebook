library(mosaic)
library(tidyverse)

lm.u <- lm(gasbill ~ temp, data=Utilities)
summary(lm.u)

lm.u.sqsq <- lm(sqrt(sqrt(gasbill)) ~ temp, data=Utilities)
summary(lm.u.sqsq)
plot(lm.u.sqsq, which=1)

b <- coef(lm.u.sqsq)

pred.u <- predict(lm.u, data.frame(temp=30), interval="prediction")
pred.u.sqsq <- predict(lm.u.sqsq, data.frame(temp=30), interval="prediction")^4

pred.u2 <- predict(lm.u, data.frame(temp=70), interval="prediction")
pred.u.sqsq2 <- predict(lm.u.sqsq, data.frame(temp=70), interval="prediction")^4


plot(gasbill ~ temp, data=Utilities)
abline(lm.u, col="hotpink")
abline(225.7804 + 26.45, -2.9704, lty=2, col="hotpink")
abline(225.7804 - 26.45, -2.9704, lty=2, col="hotpink")
curve((b[1] + b[2]*x)^4, add=TRUE, col="skyblue", lwd=2)
curve((b[1]+0.2258 + b[2]*x)^4, add=TRUE, col="skyblue", lty=2, lwd=2)
curve((b[1]-0.2258 + b[2]*x)^4, add=TRUE, col="skyblue", lty=2, lwd=2)
abline(h=pred.u, lty=2, col="hotpink")
abline(h=pred.u.sqsq, lty=2, col="skyblue")
abline(v=c(30,70), lty=2)
abline(h=pred.u2, lty=2, col="hotpink")
abline(h=pred.u.sqsq2, lty=2, col="skyblue")

plot(lm.u, which=1)
View(Utilities)

library(car)
boxCox(lm.u)



ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y~x, se=T) +
  stat_function(fun=function(x) (b[1] + b[2]*x)^4) + 
  geom_segment(aes(x=30,xend=30, y=pred.u[2], yend=pred.u[3]), alpha=0.01, lwd=3, col="hotpink") + 
  geom_segment(aes(x=30,xend=30, y=pred.u.sqsq[2], yend=pred.u.sqsq[3]), alpha=0.01, lwd=2, col="skyblue") + 
  geom_segment(aes(x=70,xend=70, y=pred.u2[2], yend=pred.u2[3]), alpha=0.01, lwd=3, col="hotpink") + 
  geom_segment(aes(x=70,xend=70, y=pred.u.sqsq2[2], yend=pred.u.sqsq2[3]), alpha=0.01, lwd=2, col="skyblue") 
  