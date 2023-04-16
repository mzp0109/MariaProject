soy <- read.csv("Data/Soy153new.csv")

##Linear Models for the Means
lm1 <- lm(Pod_mean~Height_mean, data = soy)
summary(lm1)
anova (lm1)
lm1ggplot <- ggplot(lm1, aes(y = Pod_mean, x = Height_mean)) +
  ylab("Total Pods per Plant") +
  xlab ("Plant Height (cm)") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="darkorange", se = FALSE) +
  theme_classic()
lm1ggplot

lm2 <- lm(SW_mean~Height_mean, data = soy)
summary(lm2)
anova (lm2)
lm2ggplot <- ggplot(lm2, aes(y = SW_mean, x = Height_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Plant Height (cm)") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="darkorange", se = FALSE) +
  theme_classic()
lm2ggplot

lm3 <- lm(Pod_mean~Branch_mean, data = soy)
summary(lm3)
anova (lm3)
lm3ggplot <- ggplot(lm3, aes(y = Pod_mean, x = Branch_mean)) +
  ylab("Total Pods per Plant") +
  xlab ("Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="darkorange", se = FALSE) +
  theme_classic()
lm3ggplot

lm4 <- lm(SW_mean~Pod_mean, data = soy)
summary(lm4)
anova (lm4)
lm4ggplot <- ggplot(lm4, aes(y = SW_mean, x = Pod_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Total Pods per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="darkorange", se = FALSE) +
  theme_classic() 
lm4ggplot

lm5 <- lm(SW_mean~Branch_mean, data = soy)
summary(lm5)
anova (lm5)
lm5ggplot <- ggplot(lm5, aes(y = SW_mean, x = Branch_mean)) +
  ylab("Seed Weight (g/plant)") +
  xlab ("Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="darkorange", se = FALSE) +
  theme_classic() 
lm5ggplot

fig1 <- ggarrange(lm1ggplot, lm2ggplot, lm3ggplot, lm5ggplot, lm4ggplot,  
          labels = "AUTO", 
          font.label = list(size=12, color = "black", face = "bold"),
          hjust = -5,
          align = "hv",
          nrow = 2, ncol = 3, common.legend = FALSE)
fig1

annotate_figure(fig1, top = text_grob("Visualizing Interaction of Means",
                                      face = "bold",
                                      size = 14),
                fig.lab = "Figure 1.", fig.lab.face = "bold")

##Linear Models for the standard deviations
lm6 <- lm(Pod_sd~Branch_sd, data = soy)
summary(lm6)
anova (lm6)
lm6ggplot <- ggplot(lm6, aes(y = Pod_sd, x = Branch_sd)) +
  ylab("Standard Deviation for Total Pods per Plant") +
  xlab ("Standard Deviation for Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="red", se = FALSE) +
  theme_classic() 
lm6ggplot

lm7 <- lm(Pod_sd~SW_sd, data = soy)
summary(lm7)
anova (lm7)
lm7ggplot <- ggplot(lm7, aes(y = SW_sd, x = Pod_sd)) +
  ylab("Standard Deviation for Seed Weight (g/plant)") +
  xlab ("Standard Deviation fo Total Pods per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="red", se = FALSE) +
  theme_classic() 
lm7ggplot

lm8 <- lm(Branch_sd~SW_sd, data = soy)
summary(lm8)
anova (lm8)
lm8ggplot <- ggplot(lm8, aes(y = SW_sd, x = Branch_sd)) +
  ylab("Standard Deviation for Seed Weight (g/plant)") +
  xlab ("Standard Deviation for Lateral Branches per Plant") +
  geom_point(size = 1.25) +
  geom_smooth(method = "lm", color="red", se = FALSE) +
  theme_classic()
lm8ggplot

fig2 <- ggarrange(lm6ggplot, lm8ggplot, lm7ggplot,
          labels = "AUTO", 
          font.label = list(size=12, color = "black", face = "bold"),
          hjust = -5,
          nrow = 1, ncol = 3, common.legend = FALSE)
fig2

annotate_figure(fig2, top = text_grob("Visualizing Variability Among Plants",
                                      face = "bold",
                                      size = 14),
                fig.lab = "Figure 2.", fig.lab.face = "bold")
