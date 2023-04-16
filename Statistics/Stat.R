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

##Linear Model
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

##Linear Model
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
ggplot(lm1, aes(y = Pod_mean, x = Height_mean)) +
  ylab("Total Pods per Plant") +
  xlab ("Plant Height (cm)") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

lm2 <- lm(SW_mean~Height_mean, data = soy)
summary(lm2)
anova (lm2)
ggplot(lm2, aes(y = SW_mean, x = Height_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Plant Height (cm)") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

lm3 <- lm(Pod_mean~Branch_mean, data = soy)
summary(lm3)
anova (lm3)
ggplot(lm3, aes(y = Pod_mean, x = Branch_mean)) +
  ylab("Total Pods per Plant") +
  xlab ("Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()


lm4 <- lm(SW_mean~Pod_mean, data = soy)
summary(lm4)
anova (lm4)
ggplot(lm4, aes(y = SW_mean, x = Pod_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Total Pods per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() 

lm5 <- lm(SW_mean~Branch_mean, data = soy)
summary(lm5)
anova (lm5)
ggplot(lm5, aes(y = SW_mean, x = Branch_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() 

##Linear Models for the standard deviations
lm6 <- lm(Pod_sd~Branch_sd, data = soy)
summary(lm6)
anova (lm6)
ggplot(lm6, aes(y = Pod_sd, x = Branch_sd)) +
  ylab("Standard Deviation for Total Pods per Plant") +
  xlab ("Standard Deviation for Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() 

lm7 <- lm(Pod_sd~SW_sd, data = soy)
summary(lm7)
anova (lm7)
ggplot(lm7, aes(y = SW_sd, x = Pod_sd)) +
  ylab("Standard Deviation for Seed Weight (g/plant)") +
  xlab ("Standard Deviation fo Total Pods per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() 

lm8 <- lm(Branch_sd~SW_sd, data = soy)
summary(lm8)
anova (lm8)
ggplot(lm8, aes(y = SW_sd, x = Branch_sd)) +
  ylab("Standard Deviation for Seed Weight (g/plant)") +
  xlab ("Standard Deviation for Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

prediction <- glm(SW_mean~Branch_mean+Pod_mean+Height_mean, data = soy)
prediction
summary(prediction)
