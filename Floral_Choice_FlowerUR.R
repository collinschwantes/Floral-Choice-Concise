### Looking at sex based responses to predators
library(ggplot2)
library(lme4)

#Load ambush bug data####
bugs<-read.csv("./Ambush_032715.csv",stringsAsFactors = F)

str(bugs)
#levels(abugs$Treatment)
#levels(abugs$Block)
#levels(abugs$Genus)
#levels(abugs$gender)
#levels(abugs$land)

#View(bugs)s

bugs[,c(1:3)] <- lapply(bugs[,c(1:3)], factor)

str(bugs)



### Think about flower as unit of replication 

df_visits <- as.data.frame(xtabs( (duration.s.>0) ~ Flower + sex + Treatment + Block, data = bugs[bugs$Genus == "Melissodes",]))

str(df_visits)

hist((df_visits$Freq),breaks = 30)

ggplot(data = df_visits, aes(x = Treatment, y = Freq, color = sex)) +
  geom_point(position = position_jitter(width = .2)) +
  geom_smooth(method = "glm", method.args = list(family = "Binomial")) +
  ylab("Visits per Flower")


hist( sqrt(df_visits$Freq))

slm1 <- lm(formula = sqrt(Freq) ~ sex + Treatment + sex*Treatment,data = df_visits)

plot(lm1)
anova(lm1)


lm2 <- lm(formula = sqrt(Freq) ~ sex + Treatment, data = df_visits)

anova(lm2)

plot.design(sqrt(Freq) ~ sex + Treatment, data = df_visits)

## Proportion Landing
str(bugs)

df_land <- as.data.frame(xtabs( land == "yes" ~ Flower + sex + Treatment + Block, data = bugs[bugs$Genus == "Melissodes",]))

str(df_land)

df_land$visits <- df_visits$Freq

df_land$Proportion <- NA

df_land[df_land$visits != 0,7] <- df_land[df_land$visits != 0,5]/df_land[df_land$visits != 0,6]

head(df_land)

ggplot(data = df_land,aes(y = Proportion, x = Treatment)) +
  geom_violin() +
  geom_point(position = position_jitter(.2)) +
  ylab("Proportion Landed") +
  facet_wrap(~sex)

#pollen

str(bugs)

df_pollen <- as.data.frame(xtabs( pollen == "yes" ~ Flower + sex + Treatment + Block, data = bugs[bugs$Genus == "Melissodes",]))

str(df_pollen)

df_pollen$land <- df_land$Freq

df_pollen$Proportion <- NA

df_pollen[df_pollen$land != 0,7] <- df_pollen[df_pollen$land != 0,5]/df_pollen[df_pollen$land != 0,6]

head(df_pollen)

ggplot(data = df_pollen,aes(y = Proportion, x = Treatment)) +
  geom_violin() +
  geom_point(position = position_jitter(.2)) +
  #geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion pollen|landed") +
  facet_wrap(~sex)


ggplot(data = df_pollen,aes(y = Proportion, x = Treatment)) +
  geom_violin() +
  geom_point(position = position_jitter(.2)) +
  #geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion pollen|landed") +
  facet_wrap(~sex)

#Nectar

str(bugs)

df_nectar <- as.data.frame(xtabs( nectar == "yes" ~ Flower + sex + Treatment + Block, data = bugs[bugs$Genus == "Melissodes",]))

str(df_nectar)

df_nectar$land <- df_land$Freq

df_nectar$Proportion <- NA

df_nectar[df_nectar$land != 0,7] <- df_nectar[df_nectar$land != 0,5]/df_nectar[df_nectar$land != 0,6]

head(df_nectar)

ggplot(data = df_nectar,aes(y = Proportion, x = Treatment)) +
  geom_violin() +
  geom_point(position = position_jitter(.2)) +
  geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion nectar|landed") +
  facet_wrap(~sex)

hist(df_nectar$Proportion,breaks = 10)

mell_df <- bugs[bugs$Genus == "Melissodes",]

str(mell_df)

mell_df$Poll_01 <- gsub(pattern = "yes",replacement = 1,x = mell_df$pollen)
mell_df$Poll_01 <- gsub(pattern = "no",replacement = 0,x = mell_df$Poll_01)

mell_df$Poll_01 <- as.numeric(mell_df$Poll_01)

contrasts(as.factor(mell_df$Treatment))

glm1 <- glm(formula = Poll_01 ~ as.factor(sex) + as.factor(Treatment),family = binomial(link = 'logit'),data = mell_df)

summary(glm1)

plot(glm1)

anova(glm1,test = "Chisq")



glm1 <- glm(formula = Poll_01 ~ as.factor(Treatment),family = binomial(link = 'logit'),data = mell_df[mell_df$sex == "male",])

summary(glm1)

plot(glm1)

anova(glm1,test = "Chisq")








