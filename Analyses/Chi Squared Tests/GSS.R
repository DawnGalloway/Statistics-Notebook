


library(tidyverse)
library(readr)
GSS2012 <- read_delim("Data/GSS2012.csv", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
# Don't run view may crash computer also change to tab delimeter in import box
names(GSS2012)
# http://sda.berkeley.edu/sdaweb/analysis/?dataset=gss12
x <- cbind(GSS2012$age, GSS2012$bigbang)
chisq.test(x)
table(GSS2012$age)
table(GSS2012$bigbang)
table(GSS2012$age, GSS2012$bigbang)

# send table into the function
barplot(table(GSS2012$age))
table(GSS2012$bigbang)
table(GSS2012$age, GSS2012$bigbang)

#filter out na, change order levels
GSSf <- GSS2012 %>% select(age, bigbang) %>% 
  mutate(bigbangf=case_when(
    bigbang==1 ~"True",
    bigbang==2 ~ "False",
    bigbang%in% c(0, 9) ~ "NA",
    bigbang==8 ~ "DK"),
    agef= case_when(age < 30 ~ "18 - 29",
                    age < 40 ~ "30 - 39",
                    age < 50 ~ "40 - 49",
                    age < 60 ~ "50 - 59",
                    age < 70 ~ "60 - 69",
                    age < 80 ~ "70 - 79",
                    age > 90 ~ "80 or older",
                    TRUE ~ "NA")) %>%  filter(bigbangf != "NA", agef != "NA") %>% 
  mutate(bigbangf=factor(bigbangf, levels = c("True", "False", "DK")))

barplot(table(GSS2012f$bigbangf))
barplot(table(GSS2012f$agef))
barplot(table(GSSf$agef, GSSf$bigbangf))

#null is independent
mychi <- chisq.test(table(GSSf$agef, GSSf$bigbangf))
# with a pvalue of whatever
mychi
# requiremnet
# should all be greater than five so requirement is mett
mychi$expected
# if you have a significant result why dig deeper, look at largest and smallest values this had a significantly higher (more counts than expected) negative means less counts
mychi$residuals
# make sure to filter this out add