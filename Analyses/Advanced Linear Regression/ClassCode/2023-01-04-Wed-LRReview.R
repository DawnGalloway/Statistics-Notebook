library(tidyverse)
View(airquality)
?airquality

## Histogram

hist(airquality$Temp)

ggplot(airquality, aes(x=Temp)) +
geom_histogram(binwidth=5, fill="skyblue", color="skyblue4") +
labs(title="Maximum daily...", subtitle = "May to September 1973", 
     x="Temperature in Degrees F", y="Number of Days in Temperature Range")


mean(airquality$Temp)
sd(airquality$Temp)

library(mosaic)
favstats(airquality$Temp)



## Boxplot 

boxplot(Temp ~ Month, data=airquality)

ggplot(airquality, aes(x=as.factor(Month), y=Temp)) + 
  geom_boxplot()

favstats(Temp ~ Month, data=airquality)


## Scatterplot

ggplot(airquality, aes(x=Wind, y=Temp)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F, formula=y~x)


mylm <- lm(Temp ~ Wind, data=airquality)
summary(mylm)

predict(mylm, data.frame(Wind=19))

