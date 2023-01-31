library(mosaic)
library(tidyverse)


View(KidsFeet)

#Boxplot & five number summary
# side by side boxplot y~x
#length quantitative by gender quantitative
boxplot(length ~ sex, data=KidsFeet)
# Data comes from the mtcars dataset
boxplot(KidsFeet$length ~ KidsFeet$sex, col="orange", main="Distribution of Feet Length by Sex", ylab="Length", xlab="Sex") 

# create a five - number summary table in support of the boxplot
# shortcut for pipe is ctrl + shift + m
library(pander) # makes it look more pretty (numerical summaries)
KidsFeet %>% 
  group_by(sex) %>% 
  summarise(min = min(length),
            Q1 = quantile(length, .25),
            median = median(length),
            Q3 = quantile(length, .75), 
            max = max(length)) %>% pander()


#bar plots and table counts
#counts how many of each occourrance each value accours
#Brother Palmers way
#total <- table(KidsFeet$sex)
#barplot(total)
barplot(table(KidsFeet$sex))

#create a bar plot that shows how many boys and girls feet are in the data set
table(KidsFeet$sex)

#scatterplot and correlation
#scatter plot shows relationship between, two quantititanvi variable
plot(length ~ width, data = KidsFeet)
#correlation coefficient summarized scater plot data, cor here stands for correlation
#notice passed in opposite order (x, y)
KidsFeet %>% summarise(cor(width, length))

#plot example with ggplot (if done as below already piped in data)
KidsFeet %>%  ggplot(mapping = aes(x = width, y = length)) + geom_point()


#NAs are contagious they will permiate your data
#?mean()
# pass na.rm true to remove the nas
#for mean need to add mean(data$co, na.rm = true)

#?cor (stats)
#correlation does not have an remove na, for this one must find a different options pairwise.complete.obs

auto <- filter(mtcars, am == 0)
man <-   filter(mtcars, am == 1)
mean(man$hp)

palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), pch=16, xlab="Quarter Mile Time (seconds)", ylab="Miles per Gallon", main="1974 Motor Trend Vehicles")
legend("topright", pch=16, legend=c("automatic","manual"), title="Transmission", bty='n', col=palette())
