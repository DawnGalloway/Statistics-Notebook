install.packages("plotly")
library(plotly)

x = c(5, 15, 2, 29, 35, 24, 25, 39) 
sum(x)
sum( (1:6)^2 ) 
sum(x^2)
mn <- mean(x)
var(x)
dist <-(x-mn)
dist
sum(dist^2)/7
mean(cars$dist)

sum((1:3)^2)
# i stands for individual
# xi-xbar is the deviation
# - areas don't exist
# cannot use the abs value instead of squaring because abs values aren't differentiatable
# n-1 (degrees of freedom) the very last person in has no freedom to select where they sit in the classroom
# if we try to use the same thing twice we get penalized -1
# when add up the squares and divide by n-1 we get the average(ish) of square or mean square divide sum of squares by degrees of freedom
# Yhat is the regression line, Ybar is the average flat line
# R^2 the amount of hvariation in Y that we can explain with the regression (our regression has reduced the variation around Yhat)
cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)
cars2 <- cbind(rbind(cars, 
                     cbind(speed=rep(0,50), dist=cars$dist),
                     cbind(speed=cars$speed, dist=cars.lm$fitted.values)),
               frame = rep(c(2,1,3), each=50))
plot_ly(cars2,
        x = ~speed,
        y = ~dist,
        frame = ~frame,
        type = 'scatter',
        mode = 'markers',
        showlegend = F,
        marker=list(color="firebrick")) %>%
  layout(title="Stopping Distance of 1920's Vehicles\n (cars data set)",
         xaxis=list(title="Vehicles Speed in mph (speed)"),
         yaxis=list(title="Stopping Distance in Feet (dist)")) %>%
  add_segments(x = 0, xend = 25, y = mean(cars$dist), yend = mean(cars$dist), line=list(color="gray", dash="dash", width=1), inherit=FALSE, name=TeX("\\bar{Y}"), showlegend=T) %>%
  add_segments(x = 0, xend = 25, y = sum(coef(cars.lm)*c(1,0)), yend = sum(coef(cars.lm)*c(1,25)), line=list(color="darkgray", width=1), inherit=FALSE, name=TeX("\\hat{Y}"), showlegend=T) %>%
  config(mathjax = 'cdn') %>%
  animation_opts(frame=2000, transition=1000, redraw=T)
# plot above is the SSTO this is the total amount of variability in your data
# SSR A good regression should decrease the var so SSE should b e less than SSTO 
# Sum of Square Regression Error mesured for every x even 
# only way your line could be worse than ybar is if it moved away from the data so it's not possible because Yhat is the best fit (is in the middle)
# Why does dividing SSR/SSTO give a good we want Yhat to be far from yhat we're getting more explanation if it's close to 0 then it's close to the average not telling us anything
# Proportion of variation in Y explained by the regression.

# SSE is the unexplained variability
# sum( lmObject$res^2 )
sum(cars.lm$res^2 )

# SSR is the explained variablility
# sum( (lmObject$fit - mean(YourData$Y))^2 )
cars.ssr <- sum( (cars.lm$fit - mean(cars$dist))^2 )

# SSTO
#sum( (YourData$Y - mean(YourData$Y))^2 )
cars.ssto <- sum( (cars$dist - mean(cars$dist))^2 )


# R2 is the ration between explanation and total SSR/SSTO
cars.ssr/cars.ssto

# r is square root of r^2
sqrt(cars.ssr/cars.ssto)


# Mobius
View(Orange)
orange.lm <- lm(circumference~age, Orange)
summary(orange.lm)
mean(Orange$circumference)
# yhat = 17.399650 + 


ggplot(Orange, aes(y=circumference, x=age)) +
  geom_point() +
  geom_smooth(method="lm", se=F, forumula=y~x)

# SSE is the unexplained variability
# sum( lmObject$res^2 )
sum(orange.lm$res^2 )

# SSR is the explained variablility
# sum( (lmObject$fit - mean(YourData$Y))^2 )
orange.ssr <- sum( (orange.lm$fit - mean(Orange$circumference))^2 )

# SSTO
#sum( (YourData$Y - mean(YourData$Y))^2 )
orange.ssto <- sum( (Orange$circumference - mean(Orange$circumference))^2 )


# R2 is the ration between explanation and total SSR/SSTO
r2 <-orange.ssr/orange.ssto
r2

# r is square root of r^2
sqrt(r2)

orange.ssr
orange.ssto
predict(orange.lm, data.frame(age=3*365))

# wt
mtwt.lm <- lm(mpg~wt, mtcars)
summary(mtwt.lm)

ggplot(mtcars, aes(y=mpg, x=wt)) +
  geom_point() +
  geom_smooth(method="lm", se=F, forumula=y~x)

# SSE is the unexplained variability
# sum( lmObject$res^2 )
sum(mtwt.lm$res^2 )

# SSR is the explained variablility
# sum( (lmObject$fit - mean(YourData$Y))^2 )
(mtwt.ssr <- sum( (mtwt.lm$fit - mean(mtcars$mpg))^2 ))

# SSTO
#sum( (YourData$Y - mean(YourData$Y))^2 )
(mtwt.ssto <- sum( (mtcars$mpg - mean(mtcars$mpg))^2 ))


# R2 is the ration between explanation and total SSR/SSTO
(wtr2 <-mtwt.ssr/mtwt.ssto)


# r is square root of r^2
sqrt(wtr2)

predict(mtwt.lm, data.frame(wt=3*365))

par(mfrow=c(1,3))
plot(mtwt.lm, which = 1:2)
plot(mtwt.lm$residuals)


# cyl
mtcyl.lm <- lm(mpg~cyl, mtcars)
summary(mtcyl.lm)

ggplot(mtcars, aes(y=mpg, x=cyl)) +
  geom_point() +
  geom_smooth(method="lm", se=F, forumula=y~x)

# SSE is the unexplained variability
# sum( lmObject$res^2 )
sum(mtcyl.lm$res^2 )

# SSR is the explained variablility
# sum( (lmObject$fit - mean(YourData$Y))^2 )
(mtcyl.ssr <- sum( (mtcyl.lm$fit - mean(mtcars$mpg))^2 ))

# SSTO
#sum( (YourData$Y - mean(YourData$Y))^2 )
(mtcyl.ssto <- sum( (mtcars$mpg - mean(mtcars$mpg))^2 ))


# R2 is the ration between explanation and total SSR/SSTO
(cylr2 <-mtcyl.ssr/mtcyl.ssto)


# r is square root of r^2
sqrt(cylr2)

predict(mtcyl.lm, data.frame(cyl=3*365))

par(mfrow=c(1,3))
plot(mtcyl.lm, which = 1:2)
plot(mtcyl.lm$residuals)

# hp
mthp.lm <- lm(mpg~hp, mtcars)
summary(mtcyl.lm)

ggplot(mtcars, aes(hp, x=cyl)) +
  geom_point() +
  geom_smooth(method="lm", se=F, forumula=y~x)

# SSE is the unexplained variability
# sum( lmObject$res^2 )
sum(mthp.lm$res^2 )

# SSR is the explained variablility
# sum( (lmObject$fit - mean(YourData$Y))^2 )
(mthp.ssr <- sum( (mthp.lm$fit - mean(mtcars$mpg))^2 ))

# SSTO
#sum( (YourData$Y - mean(YourData$Y))^2 )
(mthp.ssto <- sum( (mtcars$mpg - mean(mtcars$mpg))^2 ))


# R2 is the ration between explanation and total SSR/SSTO
(hpr2 <-mthp.ssr/mthp.ssto)


# r is square root of r^2
sqrt(hpr2)

par(mfrow=c(1,3))
plot(mthp.lm, which = 1:2)
plot(mthp.lm$residuals)


# Type 1 error is significant lie
# A significant result doesn't mean that it's true, it only means it's significant


Describe the data
SS an area measure is a square if we add up residuals we get zero every time 
sum of square residual
the line in the middle on the average it's the smallest sum of least squares or the least squares line
what's difference and a residual and epsilon residual obs - predicted epsilon is the obs - actual 
residual how far you think I'm from Christ epsilon is how far I am from Christ
epsilon is theoretical assumption because it's based on a truth we can't see
no other line can minimize the squares as much as the best fit

We can't get residuals until we get a line, but the line is picked by minimizing the residuals
Residuals gave you the line as a gift

R2 of 1 gives you nothing because residuals are one, but .99 is good if you get really high R squared we suspect you did sometging wrong like using Y to predict Y

proportion or variation in Y that's used by using the regression (x variable)
cool graph to make in r

not ri squared r squared  r is the correlation written R^2
Any time you square a decimal it gets smaller R^2 is stricter on the correlation
If you signed up for a tutor, a good question would be what was your grade in 425

As the line moves to the middle of the data the SSE decreases 
SSR is reserved for how far the regression line is from Ybar
best fit line
Residuals give us the line and R2
value of a regression, which is the proportion of variation in Y explained by the regression model. The  SSR/SSTO

The SSE IS the unexplained the amount that we still have in errors (we don't like SSE because it's what we still don't know)
The SSR is the part explained by the regression line (we like SSR because we understand) Subtract the R2 a barchart could show that.
R2 is the part of the variability we've come to explain or 1-the part we hcan't explani
R2 is not the portion of the data that has been explained by regression line the
It's the variability (departure of the dot from average) that is explained by the data

Why hard to understand R2, we don't know what proportion is or variability what is explained (if we have an R2 of one we've totally explained the data)

Does god ever doanything unexpected He's steady and constant and true. There is no variability in him.
                                                                                              What variance means is suddenly you're not where you used to be. God doesn't have any SSE

Could we have a small p-value and a vary large R2?
  pvalue measures the explanation compared to random chance--He's only seen it once a low slow and a million data points. But it's not normal
R2 isn't necessarily the steepness but how tight the dots are line

p-value measures whether their is a law
If there is no law then who cares whether the data follows it
If their is the law
p-value doesn't mean you found truth but you found something significant.
R2 how close the data is to the line
There are laws that exist but people don't follow them
No one doesn't fall the law of gravity

3rd What else do residuals do. 

First, residuals are the key to obtaining the "least squares estimates" of the regression parameters and. give us the line

Second, residuals are an important part in measuring the value of a regression, which is the proportion of variation in Y explained by the regression model.
tell us the line

hird, residuals give insight about how much an individual of a given x-value differs from average in their y-value.

Residuals estimate tells us sigma2
residual standard error is sqt(SSE/n-p)
MSE = SSE/(n-p) Estimate of signma2 p is parameters
Residual Standard Error is estimating the sigma of the dots
Fourth, (and this one you haven't seen yet) residuals can be used to estimate the variance parameter of a regression, i.e.,in the equation

Fifth, (and we will see this one in more detail next week) residuals can be used to determine if a linear regression model is appropriate for a given data set.

We got line from residuals tell how good it is from residuals can estimpate the model praarmeters ilke sigma whether the residuals are appropriate


SSE is all the boxes added together
MSE is the average size of the boxes mean average se the errors 


residual tells how far you are from the line

