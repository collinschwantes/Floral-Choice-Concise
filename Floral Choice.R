## This is the complete code for the beetle and ambush bug interactions

require(ggplot2)
require(lme4)
require(plyr)
require(dplyr)
require(gridExtra)

#Load ambush bug data####
bugs<-read.csv("./Ambush_032715.csv")

str(bugs)
levels(abugs$Treatment)
levels(abugs$Block)
levels(abugs$Genus)
levels(abugs$gender)
levels(abugs$land)

View(bugs)

## only abug data
Obugs <- bugs[ grepl(pattern =  "Ambush |Ambush Bug",x = bugs$Genus),]
length(Obugs$Day)
length(bugs$Day)
157/sqrt(30)

sqrt(100)/sqrt(4)



## removing rows that don't have bee data
## comparing only bee visits
abugs <- bugs[bugs$Genus != "Ambush ",]
abugs <- abugs[abugs$Genus != "Ambush Bug",]
abugs <- abugs[abugs$Genus != "BEETLE",]
abugs <- abugs[abugs$Genus != "EARWIG",]
abugs <- abugs[abugs$Genus != "fly",]
abugs <- abugs[abugs$Genus != "Parenthesis lady Beetles",]
abugs <- abugs[abugs$Genus != "red beetle",]
abugs <- abugs[abugs$Genus != "wasp",]
abugs <- abugs[abugs$Genus != "no bees",]

# Only female bees
Fabugs <- abugs[abugs$sex != "male",]

# Only Solitary Bees
SFabugs <- Fabugs[Fabugs$Genus != "Bombus",]
SFabugs <- SFabugs[SFabugs$Genus != "Apis",]

View(SFabugs)
#### Load beetle data
beets<-read.csv("./beetle_031915.csv")

str(beets)
View(beets)

## only beetle data
Obeets <- beets[beets$Treatment == "treated",]
ObeetsL <- Obeets[ grepl(pattern =  "beetle left on treated|beetle left on treatment|beetle left",x = Obeets$Genus),]
length(ObeetsL$Day)
length(Obeets$Day)

# the duration of a beetle leaving a flower was recorded as the total amount of the
# observation the beetle spent off the flower. So 600 s - time away = obs duration and 
# the amount of time the beetle spent of the flower 

mean_se(c(rep(600,88-22),600-ObeetsL$duration.s.))/60

8.556 - 8.254

.302*60
.556*60

#SD / sqrt (Number of obvs) = standard error
187.2/sqrt(30)

3*60

## removing rows that don't have bee data
abeets <- beets[ beets$Genus != "beetle left on treated",]
abeets <- abeets[abeets$Genus != "beetle left on treatment",]
abeets <- abeets <- abeets[abeets$Genus != "no bees",]
abeets <- abeets <- abeets[abeets$Genus != "no bees ",]
abeets <- abeets[abeets$Genus != "beetle left",]

abeets
abeets <- droplevels(abeets)
str(abeets)
View(abeets)

#remove incomplete rows 
abeets <- abeets[ abeets$nectar != "",]
abeets <- abeets[ abeets$pollen != "",]
abeets <- abeets[ abeets$land != "",]

#male bee visit durations
mean_se(abeets$duration.s.[abeets$gender == "male"])

mean(abeets$duration.s.[abeets$gender == "male"]) 
  std_er(abeets$duration.s.[abeets$gender == "male"])



mean_se(abeets$duration.s.[abeets$gender == "female"])


#select only femalte bees
Fabeets <- abeets[ abeets$gender == "female",]
View(Fabeets)

#remove social bees
SFabeets <- Fabeets[ Fabeets$Genus != "bombus",]
SFabeets <- SFabeets[ SFabeets$Genus != "apis",]
SFabeets <-  droplevels(SFabeets)
View(SFabeets)

####### PRELANDING BEHAVIORS ######

# Visitation Rate beetle

#dataset with visits per pair
VSFabeets <- ddply(SFabeets,.(Treatment,Block,Day,Pair),summarise,
                   Visit = length(duration.s.))
hist(VSFabeets$Visit, breaks  = 10)

visit.trmtB1<- glmer( Visit ~ Treatment + (1|Pair) + (1|Day) + (1|Block), 
                      data=VSFabeets, 
                      family= poisson)
summary(visit.trmtB1)
plot(allEffects(visit.trmtB1))

var(log(VSFabeets$Visit))

visit.trmtB2<- glmer( Visit ~ Treatment + (1|Day) + (1|Pair), 
                      data=VSFabeets, 
                      family= poisson)

summary(visit.trmtB2)
plot(allEffects(visit.trmtB2))

visit.trmtB3<- glmer( Visit ~ Treatment + (1|Pair), 
                      data=VSFabeets, 
                      family= poisson)

summary(visit.trmtB3)
plot(allEffects(visit.trmtB3))

visit.trmtB4<- glm( Visit ~ Treatment, 
                    data=VSFabeets, 
                    family= poisson)

summary(visit.trmtB4)

#Visitation Rate Ambush Bug
#dataset with visits per pair
VSFabugs <- ddply(SFabugs,.(Treatment,Block,Day,Pair),summarise,
                   Visit = length(duration.s.))
hist(VSFabugs$Visit, breaks  = 10)

visit.trmtA1<- glmer( Visit ~ Treatment + (1|Pair) + (1|Day) + (1|Block), 
                      data=VSFabugs, 
                      family= poisson)
summary(visit.trmtA1)
plot(allEffects(visit.trmtB1))

var(log(VSFabugs$Visit))

visit.trmtA2<- glmer( Visit ~ Treatment + (1|Day) + (1|Pair), 
                      data=VSFabugs, 
                      family= poisson)

summary(visit.trmtA2)
plot(allEffects(visit.trmtA2))

visit.trmtA3<- glmer( Visit ~ Treatment + (1|Pair), 
                      data=VSFabugs, 
                      family= poisson)

summary(visit.trmtA3)
plot(allEffects(visit.trmtA3))

visit.trmtA4<- glm( Visit ~ Treatment, 
                    data=VSFabugs, 
                    family= poisson)

summary(visit.trmtA4)

###### Plots for Visit ##########

#Effect of floral occupancy on Visitation Rate

SFabeets$Genus

VSFTPabeets <- ddply(SFabeets,.(Treatment,Pair,Day),summarise,
                     Visit = length(duration.s.))

beetSFP <- ddply(VSFTPabeets,.(Pair,Treatment),summarise,
                 N = length(Visit),
                 Visits = mean(Visit),
                 sd = sd(Visit),
                 se = sd(Visit)/sqrt(N),
                 Occupant = "Beetle")

VSFTPabugs <- ddply(SFabugs,.(Treatment,Pair,Day),summarise,
                    Visit = length(duration.s.))

AbugSFP <- ddply(VSFTPabugs,.(Pair,Treatment),summarise,
                 N = length(Visit),
                 Visits = mean(Visit),
                 sd = sd(Visit),
                 se = sd(Visit)/sqrt(N),
                 Occupant = "Ambush Bug")

VSFP_comp <- rbind(beetSFP,AbugSFP)

str(VSFP_comp)

labtxt <- data.frame(Pair = rep(.75,2), Visits = rep(15.25,2), label = c("C","D"), Occupant = as.factor(c("Ambush Bug","Beetle")) )

Visit.plot.pair <- ggplot(VSFP_comp, aes(x = as.factor(Pair), y = Visits, ymax = max(Visits))) + 
  theme_bw(base_size = 20, base_family = "") +
  theme(legend.justification=c(1,0), legend.position=c(1,0.5)) +
  theme(panel.grid.minor= element_blank(),
        panel.grid.major=element_line(colour = "#111111",size = 0.1)) +
  geom_point(aes(shape = Treatment), 
             na.rm = T,
             position = position_dodge(.9), 
             size = 5,
             stat = "identity") +
  geom_errorbar(aes(group = Treatment,
                    ymin = (Visits - se), 
                    ymax = (Visits + se), 
                    width = .2),
                na.rm =T,
                stat = "identity",
                position = position_dodge(width = .9)) +
  scale_shape_manual(values=c(1,19),
                     name  ="Occupancy",
                     breaks=c("control", "treated"),
                     labels=c("Absent", "Present")) +
  xlab("Observation") +
  ylab("Visits") +
  expand_limits(y=c(0,15)) +
  facet_wrap(~Occupant,nrow = 1)+ 
  theme(strip.background = element_rect(colour="#111111", fill="#ffffff")) +
  geom_text(data = labtxt,aes(Pair,Visits,label = label, group = NULL), size = 6)
  
# Visits by treatment
VSFTPabeets <- ddply(SFabeets,.(Treatment,Pair,Day),summarise,
                     Visit = length(duration.s.))

beetSFT <- ddply(VSFTPabeets,.(Treatment),summarise,
                 N = length(Visit),
                 Visits = mean(Visit),
                 sd = sd(Visit),
                 se = sd(Visit)/sqrt(N),
                 Occupant = "Beetle")

VSFTPabugs <- ddply(SFabugs,.(Treatment,Pair,Day),summarise,
                    Visit = length(duration.s.))

AbugSFT <- ddply(VSFTPabugs,.(Treatment),summarise,
                 N = length(Visit),
                 Visits = mean(Visit),
                 sd = sd(Visit),
                 se = sd(Visit)/sqrt(N),
                 Occupant = "Ambush Bug")

VSFT_comp <- rbind(beetSFT,AbugSFT)

#label df

labtxt1 <- data.frame(Treatment = rep(.5,2), Visits = rep(5.75,2), label = c("A","B"), Occupant = as.factor(c("Ambush Bug","Beetle")) )

Visit.t.plot <- ggplot(VSFT_comp, aes(x=Treatment, y=Visits)) + 
  geom_errorbar(aes(ymin=Visits-se, ymax=Visits+se), width=.1) +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor= element_blank(),
        panel.grid.major=element_line(colour = "#111111",size = 0.1)) +
  geom_point(size = 5) +
  scale_x_discrete(breaks=c("control", "treated"),
                   labels=c("Absent", "Present")) +
  xlab("Occupancy") +
  ylab("Visits") +
  expand_limits(y = c(0,6)) +
  facet_wrap(~Occupant, nrow=1) + 
  theme(strip.background = element_rect(colour="#111111", fill="#ffffff")) +
  geom_text(data = labtxt1,aes(Treatment,Visits,label = label, group = NULL), size = 6)



g <- arrangeGrob(Visit.t.plot, Visit.plot.pair, nrow=2)

ggsave(filename = "./figures/visit_plot.eps",device = "eps",plot = g,units = "in",width = 12,height = 8)

########Chi square test for landing ##########
chisq.test(x = SFabeets$land, y = SFabeets$Treatment)

Land_beets <- ggplot(SFabeets, aes(x = land, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("Land") +
  ylim(0,30) +
  ggtitle("Landing Given Beetle Presence")

chisq.test(x = SFabugs$land, y = SFabugs$Treatment)

Land_abugs <- ggplot(SFabugs, aes(x = land, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("Land") +
  ggtitle("Landing Given Ambush Bug Presence")

##################### Post Landing behavior##############


########## Chi square test for collecting nectar ####
chisq.test(x = SFabeets$nectar, y = SFabeets$Treatment)

ggplot(SFabeets, aes(x = nectar, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("Nectar") +
  ylim(0,30) +
  ggtitle("Nectaring Given Beetle Presence")

chisq.test(x = SFabugs$nectar, y = SFabugs$Treatment)

ggplot(SFabugs, aes(x = nectar, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("Nectar") +
  ylim(0,80) +
  ggtitle("Nectaring Given Ambush Bug Presence")

########## Chi square for collecting pollen #### 

chisq.test(x = SFabeets$pollen, y = SFabeets$Treatment)

ggplot(SFabugs, aes(x = pollen, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("pollen") +
  ylim(0,80) +
  ggtitle("Collecting Pollen Given Beetle Presence")
  
chisq.test(x = SFabugs$pollen, y = SFabugs$Treatment)

ggplot(SFabeets, aes(x = pollen, fill = Treatment)) +
  geom_bar( position=position_dodge()) +
  xlab("pollen") +
  ylim(0,30) +
  ggtitle("Collecting Pollen Given Ambush Bug Presence")
  

#### VISIT DURATION #####


logbeetSF <- ddply(SFabeets,.(Treatment),summarise,
                   N = length(duration.s.),
                   Duration.s. = mean(log(duration.s.)),
                   sd = sd(log(duration.s.)),
                   se = sd(log(duration.s.))/sqrt(N),
                   Occupant = "Beetle")


logAbugSF <- ddply(SFabugs,.(Treatment),summarise,
                   N = length(duration.s.),
                   Duration.s. = mean(log(duration.s.)),
                   sd = sd(log(duration.s.)),
                   se = sd(log(duration.s.))/sqrt(N),
                   Occupant = "Ambush Bug") 

beetSF <- ddply(SFabeets,.(Treatment),summarise,
                N = length(duration.s.),
                Duration.s. = mean(duration.s.),
                sd = sd(duration.s.),
                se = sd(duration.s.)/sqrt(N),
                Occupant = "Beetle")


AbugSF <- ddply(SFabugs,.(Treatment),summarise,
                N = length(duration.s.),
                Duration.s. = mean(duration.s.),
                sd = sd(duration.s.),
                se = sd(duration.s.)/sqrt(N),
                Occupant = "Ambush Bug") 


SF_comp <- rbind(beetSF,AbugSF)

labtxt2 <- data.frame(Treatment = rep(.5,2), Duration.s. = rep(14,2), label = c("A","B"), Occupant = as.factor(c("Ambush Bug","Beetle")) )

#Effect of Floral occupant on female solitary bee visit duration
Duration.plot <- ggplot(SF_comp, aes(x=Treatment, y= Duration.s. )) + 
  geom_errorbar(aes(ymin=Duration.s.-se, ymax=Duration.s.+se), width=.1) +
  theme_bw(base_size = 20, base_family = "") +
  scale_x_discrete(breaks=c("control", "treated"),
                   labels=c("Absent", "Present")) +
  geom_point(size = 5) +
  xlab("Occupancy") +
  ylab("Duration (s)") +
  expand_limits(y = c(0,8.5)) + 
  facet_wrap(~Occupant, nrow=1) + 
  theme(strip.background = element_rect(colour="#111111", fill="#ffffff")) +
  geom_text(data = labtxt2,aes(Treatment,Duration.s.,label = label, group = NULL), size = 6)


# Duration by pair
log_beetDSFP <- ddply(SFabeets,.(Treatment,Pair),summarise,
                      N = length(duration.s.),
                      Duration.s. = mean(log(duration.s.)),
                      sd = sd(log(duration.s.)),
                      se = sd(log(duration.s.))/sqrt(N),
                      Occupant = "Beetle")


log_AbugDSFP <- ddply(SFabugs,.(Treatment,Pair),summarise,
                      N = length(duration.s.),
                      Duration.s. = mean(log(duration.s.)),
                      sd = sd(log(duration.s.)),
                      se = sd(log(duration.s.))/sqrt(N),
                      Occupant = "Ambush Bug") 

beetDSFP <- ddply(SFabeets,.(Treatment,Pair),summarise,
                  N = length(duration.s.),
                  Duration.s. = mean(duration.s.),
                  sd = sd(duration.s.),
                  se = sd(duration.s.)/sqrt(N),
                  Occupant = "Beetle")


AbugDSFP <- ddply(SFabugs,.(Treatment,Pair),summarise,
                  N = length(duration.s.),
                  Duration.s. = mean(duration.s.),
                  sd = sd(duration.s.),
                  se = sd(duration.s.)/sqrt(N),
                  Occupant = "Ambush Bug") 



DSFP_comp <- rbind(beetDSFP,AbugDSFP)




Duration.plot.pair <- ggplot(DSFP_comp, aes(x = as.factor(Pair), y = Duration.s.)) + 
  theme_minimal(base_size = 20, base_family = "") +
  theme(legend.justification=c(1,0), legend.position=c(1,0.5)) +
  geom_point(aes(shape = Treatment), 
             na.rm = T,
             position = position_dodge(.9), 
             size = 5,
             stat = "identity") +
  geom_errorbar(aes(group = Treatment,
                    ymin = (Duration.s. - se), 
                    ymax = (Duration.s. + se), 
                    width = .2),
                na.rm =T,
                stat = "identity",
                position = position_dodge(width = .9)) +
  scale_shape_manual(values=c(1,19),
                     name  ="Occupancy",
                     breaks=c("control", "treated"),
                     labels=c("Absent", "Present")) +
  xlab("Observation") +
  ylab("Duration (s)") +
  expand_limits(y=c(0,15)) +
  facet_wrap(~Occupant, nrow=1)


grid.arrange( Duration.plot,Duration.plot.pair, nrow=2)

#models

# LMM of duration by day with random effects
hist(log(SFabugs$duration.s.), breaks = 100)
qqnorm?
AM0 <- lm(log(duration.s.) ~ 1, data = SFabugs)
AM1 <- lm(log(duration.s.) ~ Treatment, data = SFabugs)
AM2 <- lmer(log(duration.s.) ~ Treatment + (1|Block) + (1|Day) + (1|Pair), data = SFabugs)
AM3 <- lmer(log(duration.s.) ~ Treatment + (1|Block) + (1|Pair), data = SFabugs)
AM4 <- lmer(log(duration.s.) ~ Treatment + (1|Pair), data = SFabugs, REML = F)
AM5 <- lmer(log(duration.s.) ~ 1 + (1|Pair), data = SFabugs)
summary(AM4)
plot(AM1)
AIC(AM0,AM1,AM2, AM3,AM4,AM5)
plot(allEffects(AM6))
anova(AM4,AM5)
anova(AM4, type = 1)

(0.55/3.23)*100

exp(-.44)

#full model is too big for number of data points 
BM1 <- lmer(log(duration.s.) ~ Treatment + (1|Day) + (1|Pair) + (1|Block),data = SFabeets)
BM2 <- lmer(log(duration.s.) ~ Treatment + (1|Pair) + (1|Block), data = SFabeets)
BM3 <- lmer(log(duration.s.) ~ Treatment + (1|Pair), data = SFabeets)
BM4 <- lmer(log(duration.s.) ~ 1 + (1|Pair), data = SFabeets)
summary(BM3)



BM7 <- lm(log(duration.s.) ~ Treatment, data = SFabeets)
summary(BM7)
anova(BM7,BM8)
plot(BM7)

AIC( BM7, BM8)

anova(BM6)

BM8 <- lm(log(duration.s.) ~ 1, data = SFabeets)
anova(BM8)
plot.design(duration.s. ~  Treatment + as.factor(Pair) + as.factor(Day) + Block, data = SFabeets)
plot.design(duration.s. ~  Treatment + as.factor(Pair) + as.factor(Day) + Block, data = SFabugs)

#block b was the southern block


#### Extra plots ##### 

#plots of duration 
ggplot(SFabeets, aes(x=Treatment, y= duration.s., color= Treatment)) + 
  geom_jitter(position=position_jitter(width=.04, height=0)) + 
  scale_y_log10() 

ggplot(SFabugs, aes(x=Treatment, y= duration.s., color= Treatment)) + 
  geom_jitter(position=position_jitter(width=.04, height=0)) + 
  scale_y_log10() 



## GGPLOT of means over the 3 days
abts <- summarySE(abeets, measurevar="duration.s.", groupvars=c("Treatment","Day"))
abts

ggplot(abts, aes(x=Day, y=duration.s., colour=Treatment)) + 
  geom_errorbar(aes(ymin=duration.s.-se, ymax=duration.s.+se), width=.1) +
  geom_line() +
  geom_point()




## t-test of beetle observations
cbeets <- beets[beets$Treatment == "control", ]
cbeets <- cbeets[cbeets$Genus != "beetle left on treated",]
cbeets <- cbeets[cbeets$Genus != "beetle left on treatment",]
cbeets <- cbeets <- cbeets[cbeets$Genus != "no bees",]
tbeets <- beets[beets$Treatment == "treated", ]
tbeets <- tbeets[tbeets$Genus != "beetle left",]
tbeets <- tbeets[tbeets$Genus != "no bees",]

t.test(tbeets$duration.s.,cbeets$duration.s.) #t.test assumes normal distribution

hist(tbeets$duration.s.)
hist(cbeets$duration.s.)

### Summary Tables ######

sociala <- c(Apis = "Eusocial", Bombus ="Eusocial", Halictus = "Eusocial")
abugs$Genus <- as.character(abugs$Genus)
Socialitya <-unname(sociala[abugs$Genus])
Socialitya[is.na(Socialitya)] <- "Solitary"
abugs$Sociality <- as.factor(Socialitya)

#Table
sumTablea <- ddply(abugs,.(Genus,Sociality),summarise,
                   Visits = length(duration.s.),
                   Duration.mean = mean(duration.s.),
                   #sd = sd(duration.s.),
                   se = sd(duration.s.)/sqrt(Visits), 
                   Pollen = length(pollen[pollen == "yes"]),
                   Nectar = length(nectar[nectar == "yes"]),
                   Attack = length(attack[attack == "yes"]),
                   Male = length(sex[sex == "male"]),
                   Female = length(sex[sex == "female"]),
                   Present = length(Treatment[Treatment == "treated"]),
                   Absent = length(Treatment[Treatment == "control"]))

sumTableaT_land <- ddply(SFabugs,.(Treatment,land),summarise,
                         Visits = length(duration.s.),
                         Duration.mean = mean(duration.s.),
                         sd = sd(duration.s.),
                         se = sd(duration.s.)/sqrt(Visits), 
                         Pollen = length(pollen[pollen == "yes"]),
                         Nectar = length(nectar[nectar == "yes"]),
                         Attack = length(attack[attack == "yes"]))
with(sumTableaT_land,c(sum(Visits),sum(Pollen),sum(Nectar),sum(Attack),mean(Duration.mean)) )                     
#Land = length(land[land == "yes"]))
#Male = length(sex[sex == "male"]),
#Female = length(sex[sex == "female"]))
#Treated = length(Treatment[Treatment == "treated"]),
#Control = length(Treatment[Treatment == "control"]))

levels(abeets$Genus)
abeets$Genus <- capitalize(abeets$Genus)

socialb <- c(Apis = "Eusocial", Bombus = "Eusocial", Halictus = "Eusocial")
Socialityb <-unname(socialb[abeets$Genus])
Socialityb[is.na(Socialityb)] <- "Solitary"
abeets[, "Sociality"] <- as.factor(Socialityb)
abeets$Sociality
str(abeets)

sumTableb <- ddply(abeets,.(Genus,Sociality),summarize,
                   Visits = length(duration.s.),
                   Duration.mean = mean(duration.s.),
                   #sd = sd(duration.s.),
                   se = sd(duration.s.)/sqrt(Visits), 
                   Pollen = length(pollen[pollen == "yes"]),
                   Nectar = length(nectar[nectar == "yes"]),
                   Attack = length(attack[attack == "yes"]),
                   Male = length(gender[gender == "male"]),
                   Female = length(gender[gender == "female"]),
                   Present = length(Treatment[Treatment == "treated"]),
                   Absent = length(Treatment[Treatment == "control"]))

sumTablebT <- ddply(SFabeets,.(Treatment,land),summarise,
                    Visits = length(duration.s.),
                    Duration.mean = mean(duration.s.),
                    sd = sd(duration.s.),
                    se = sd(duration.s.)/sqrt(Visits), 
                    Pollen = length(pollen[pollen == "yes"]),
                    Nectar = length(nectar[nectar == "yes"]),
                    Attack = length(attack[attack == "yes"]))
#Land = length(land[land == "yes"]))
#Male = length(gender[gender == "male"]),
#Female = length(gender[gender == "female"]))
#Treated = length(Treatment[Treatment == "treated"]),
#Control = length(Treatment[Treatment == "control"]))

sumThtml <- kable(sumTable, format = "html",digits = 2)
write(sumThtml, file = "Summary Table Bees")
write(sumThtml,file='Summary table.html')



# Table given that they land

sumTableaT_land <- ddply(Fabugs,.(Treatment,land),summarise,
                         Visits = length(duration.s.),
                         Duration.mean = mean(duration.s.),
                         Pollen = length(pollen[pollen == "yes"]),
                         Nectar = length(nectar[nectar == "yes"]),
                         Attack = length(attack[attack == "yes"]))

with(SFabugs[SFabeets$Treatment == "treated",], length(duration.s.)
     
    
     
     
     