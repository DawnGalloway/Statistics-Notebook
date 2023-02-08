lm.mt <- lm(mpg ~ qsec, data=mtcars)
plot(mpg ~ qsec, data=mtcars)
abline(lm.mt)

plot(lm.mt, which=1)




plot(drat ~ wt, data=mtcars)
lm2 <- lm(drat ~ wt, data=mtcars)
abline(lm2)

plot(lm2, which=1)


plot(height ~ age, data=Loblolly)
lmlob <- lm(height ~ age, data=Loblolly)
abline(lmlob)

plot(lmlob, which=1)

summary(lmlob)

plot(circumference ~ age, data=Orange)





boxplot(islands, col="forestgreen")
boxplot(log(islands), col="forestgreen")



library(mosaicData)
View(Utilities)
?Utilities

plot(gasbill ~ temp, data=Utilities)

gas.lm <- lm(gasbill ~ temp, data=Utilities)

abline(gas.lm)

plot(gas.lm, which=1)


gas.lm.log <- lm(log(gasbill) ~ temp, data=Utilities)

plot(log(gasbill) ~ temp, data=Utilities)

abline(gas.lm.log)
plot(gas.lm.log, which=1)
summary(gas.lm.log)


plot(gasbill ~ temp, data=Utilities)
curve(exp(6.031885 - 0.041435*x), add=TRUE)







