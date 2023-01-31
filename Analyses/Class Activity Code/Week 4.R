# Ho: median of boys feet length = median of girls foot length
# Ha: median not equal
library(mosaic)
wilcox.test(length~sex, data=KidsFeet)
# alpha = .05, p-value = .0836 >.05, fail to reject the null
# Insufficient evidence to conclude that median boys feet length is different than girl median foot length.