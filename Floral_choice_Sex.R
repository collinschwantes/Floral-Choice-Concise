### Looking at sex based responses to predators


#Load ambush bug data####
bugs<-read.csv("./Ambush_032715.csv",stringsAsFactors = F)

str(bugs)
#levels(abugs$Treatment)
#levels(abugs$Block)
#levels(abugs$Genus)
#levels(abugs$gender)
#levels(abugs$land)

#View(bugs)

bugs[,c(1:3)] <- lapply(bugs[,c(1:3)], factor)

str(bugs)



### Think about observation as unit of replication 

df_visits <- as.data.frame(xtabs( (duration.s.>0) ~ sex + Pair + Block+ Treatment + Day, data = bugs[bugs$Genus == "Melissodes",]))

str(df_visits)

hist((df_visits$Freq),breaks = 30)

ggplot(data = df_visits, aes(x = Treatment, y = Freq, color = sex)) +
  geom_point(position = position_jitter(width = .2)) +
  geom_smooth(method = "glm", method.args = list(family = "Binomial")) +
  ylab("Visits per Observation")


hist( sqrt(df_visits$Freq))

slm1 <- lm(formula = sqrt(Freq) ~ sex + Treatment + sex*Treatment,data = df_visits)

plot(lm1)
anova(lm1)


lm2 <- lm(formula = sqrt(Freq) ~ sex + Treatment, data = df_visits)

asinas (0)
anova(lm2)

plot.design(sqrt(Freq) ~ sex + Treatment, data = df_visits)

## Proportion Landing
str(bugs)

df_land <- as.data.frame(xtabs( land == "yes" ~ sex + Pair + Block+ Treatment + Day, data = bugs[bugs$Genus == "Melissodes",]))

str(df_land)

df_land$visits <- df_visits$Freq

df_land$Proportion <- NA

df_land[df_land$visits != 0,8] <- df_land[df_land$visits != 0,6]/df_land[df_land$visits != 0,7]

head(df_land)

ggplot(data = df_land,aes(y = Proportion, x = as.numeric(Pair), color = Treatment)) +
  geom_point(position = position_jitter(.2)) +
  geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion Landed") +
  facet_wrap(~sex)

#pollen

str(bugs)

df_pollen <- as.data.frame(xtabs( pollen == "yes" ~ sex + Pair + Block+ Treatment + Day, data = bugs[bugs$Genus == "Melissodes",]))

str(df_pollen)

df_pollen$land <- df_land$Freq

df_pollen$Proportion <- NA

df_pollen[df_pollen$land != 0,8] <- df_pollen[df_pollen$land != 0,6]/df_pollen[df_pollen$land != 0,7]

head(df_pollen)

ggplot(data = df_pollen,aes(y = Proportion, x = as.numeric(Pair), color = Treatment)) +
  geom_point(position = position_jitter(.2)) +
  geom_smooth(method = "glm",method.args= list(family = "binomial")) +
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

df_nectar <- as.data.frame(xtabs( nectar == "yes" ~ sex + Pair + Block+ Treatment + Day, data = bugs[bugs$Genus == "Melissodes",]))

str(df_nectar)

df_nectar$land <- df_land$Freq

df_nectar$Proportion <- NA

df_nectar[df_nectar$land != 0,8] <- df_nectar[df_nectar$land != 0,6]/df_nectar[df_nectar$land != 0,7]

head(df_nectar)

ggplot(data = df_nectar,aes(y = Proportion, x = as.numeric(Pair), color = Treatment)) +
  geom_point(position = position_jitter(.2)) +
  geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion nectar|landed") +
  facet_wrap(~sex)


ggplot(data = df_nectar,aes(y = Proportion, x = Treatment)) +
  geom_violin() +
  geom_point(position = position_jitter(.2)) +
  #geom_smooth(method = "glm",method.args= list(family = "binomial")) +
  ylab("Proportion nectar|landed") +
  facet_wrap(~sex)


