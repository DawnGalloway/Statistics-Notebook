library(mosaic)
library(car)
library(tidyverse)

View(KidsFeet)

# By default the alternative is !
t.test(x = KidsFeet$length, mu = 28)

##### Paired ? #####
# Calculate the difference for each row, and then do a one sample t-test
newdata <- KidsFeet %>% mutate(paired_diffs = length - 3 * width)
# Didn't have to have mu = 0 because it's the default
t.test(x = newdata$paired_diffs, mu = 0)
# There's sufficient evidence of length and 3 times width is not 0?

# Do the paired test right within the t.test command
t.test(x = KidsFeet$length, y = KidsFeet$width * 3, paired = TRUE)
# Evidence length is not equal to three times the width

##### Independent t-tests #####
# Hard Way
# square bracket allow you to subset the rows!
# Careful use == or you will ruin your dataset
t.test(x = KidsFeet$length[KidsFeet$sex == "B"], 
       y = KidsFeet$length[KidsFeet$sex == "G"])
# We have insufficient evidence to coclude that the mean boy foot length is less than girl
# Easy Way, if data is nice and tidy
# Right hand side is 1 for 1 sample, or factor with 2 levels
t.test(KidsFeet$length ~ KidsFeet$sex)
# Same result as hard way

##### QQ Plots #####
# Are not a graphical summary only for the purpose of making sure that data is normal
# The car package is better because it has the dashed lines to help you see if the points are inside the boundaries
install.packages("car")
library(car)

qqPlot(KidsFeet$length ~ KidsFeet$sex)
# for one plot one vector
qqPlot(KidsFeet$length)


# Session > restart R because he overwrote his data
# Clicking on broom cleans out all the data