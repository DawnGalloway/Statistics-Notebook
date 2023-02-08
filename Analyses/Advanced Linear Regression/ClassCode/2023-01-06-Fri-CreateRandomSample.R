library(tidyverse)

ggplot(airquality, aes(x=Wind, y=Temp)) + 
  geom_point(fill="skyblue", pch=21, color="skyblue4") + 
  geom_smooth(method="lm", se=F, formula=y~x, color="skyblue4") + 
  labs(title="Max...", subtitle="May...", 
       y="Temp...", x="Average...")

mylm <- lm(Temp ~ Wind, data=airquality)
summary(mylm)






set.seed(101) #Allows us to always get the same "random" sample
#Change to a new number to get a new sample

n <- 153 #set the sample size

X_i <- runif(n, 15, 45) #Gives n random values from a uniform distribution between 15 to 45.

beta0 <- 89 #Our choice for the y-intercept. 

beta1 <- -1.25 #Our choice for the slope. 

sigma <- 1 #Our choice for the std. deviation of the error terms.

epsilon_i <- rnorm(n, 0, sigma) #Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.

Y_i <- beta0 + beta1*X_i + epsilon_i #Create Y using the normal error regression model

fabData <- data.frame(y=Y_i, x=X_i) #Store the data as data

View(fabData) 

#In the real world, we begin with data (like fabData) and try to recover the model that (we assume) was used to created it.

fab.lm <- lm(y ~ x, data=fabData) #Fit an estimated regression model to the fabData.

summary(fab.lm) #Summarize your model. 

plot(y ~ x, data=fabData, ylim=c(20,100)) #Plot the data.

abline(fab.lm) #Add the estimated regression line to your plot.

# Now for something you can't do in real life... but since we created the data...

abline(beta0, beta1, lty=2) #Add the true regression line to your plot using a dashed line (lty=2). 

legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") #Add a legend to your plot specifying which line is which.
