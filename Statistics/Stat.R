library (ggplot2)
library (lme4)
library(emmeans)
library(multcomp)
library (Hmisc)
library("PerformanceAnalytics")

soy <- read.csv("Soy153new.csv")

soymean <- soy[, c(3,5,7,9)]

soysd <- soy [, c(4,6,8,10)]

##Basic Stat
summary(soymean)

summary(soysd)

## Correlation
cormean <- cor(soymean)
corsd <- cor (soysd)

##Mean
rcormean <- rcorr(as.matrix(soymean))
rcormean$P
rcormean$r

chart.Correlation(soymean, histogram=TRUE, pch=19)

##SD
rcorsd <- rcorr(as.matrix(soysd))
rcorsd$P
rcorsd$r

chart.Correlation(soysd, histogram=TRUE, pch=19)


##Linear Models for the Means
lm1 <- lm(Pod_mean~Height_mean, data = soy)
summary(lm1)
anova (lm1)

lm2 <- lm(SW_mean~Height_mean, data = soy)
summary(lm2)
anova (lm2)

lm3 <- lm(Pod_mean~Branch_mean, data = soy)
summary(lm3)
anova (lm3)

lm4 <- lm(SW_mean~Pod_mean, data = soy)
summary(lm4)
anova (lm4)

lm5 <- lm(SW_mean~Branch_mean, data = soy)
summary(lm5)
anova (lm5)

##Linear Models for the standard deviations
lm6 <- lm(Pod_sd~Branch_sd, data = soy)
summary(lm6)
anova (lm6)

lm7 <- lm(Pod_sd~SW_sd, data = soy)
summary(lm7)
anova (lm7)

lm8 <- lm(Branch_sd~SW_sd, data = soy)
summary(lm8)
anova (lm8)


prediction <- glm(SW_mean~Branch_mean+Pod_mean+Height_mean, data = soy)
prediction
summary(prediction)
