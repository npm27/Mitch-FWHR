####Set up####
##load libraries
library(memisc)
library(reshape)
library(ez)
library(lme4)
library(car)
library(effects) #maybe use this for interactions?
library(ggplot2) #plot the things!
library(emmeans)

##turn of scientific notation
options(scipen = 999)

##load data
dat = as.data.set(spss.system.file("Study 3.sav"))
dat = as.data.frame(dat)

##see what we're working with
summary(dat)
length(unique(dat$id))

##Need to get the data in the right shape

##for the PCAT items, going to assume that the order is the same as Hofer et al. (2018)
#1-4 are protection items ##Mitch, please correct me if I'm wrong!
#5-10 are nurture items

####Build the datasets####
##dat_scale will be used for Parenting motives analysis (SEX x fWHR x MOTIVE)
dat_scale = dat[ , c(1:11, 15:24, 28:37, 41:50, 54:63, 67:76, 80:89, 93:102, 106:115, 119:128, ##HIGH
                     132:141, 145:154, 158:167, 171:180, 184:193, 197:206, 210:219, 223:232, 236:245, 249:258, ##LOW
                     262)] ##Sex

##get into long format
dat_scale = melt(dat_scale, id.vars = c("id", "Sex"))

#Make the fWHR variable
dat_scale$fWHR = c(rep("High", nrow(dat_scale) / 2), rep("Low", nrow(dat_scale) / 2))

#Make the motive variable
dat_scale$variable = gsub("_2", "P", dat_scale$variable)
dat_scale$variable = gsub("_3", "P", dat_scale$variable)
dat_scale$variable = gsub("_4", "P", dat_scale$variable)
dat_scale$variable = gsub("_5", "N", dat_scale$variable)
dat_scale$variable = gsub("_6", "N", dat_scale$variable)
dat_scale$variable = gsub("_7", "N", dat_scale$variable)
dat_scale$variable = gsub("_8", "N", dat_scale$variable)
dat_scale$variable = gsub("_9", "N", dat_scale$variable)
dat_scale$variable = gsub("_10", "N", dat_scale$variable)

dat_scale$variable = gsub("_1", "P", dat_scale$variable)

dat_scale$variable = sub('.*(?=.$)', '',
                         dat_scale$variable,
                         perl = T)

colnames(dat_scale)[3] = "Motive"
colnames(dat_scale)[4] = "Score"

dat_scale$Score = as.numeric(dat_scale$Score)

##dat_Context will be used for Mating Interest analysis (SEX x fWHR x CONTEXT)
dat_Context = dat[ , c(1, 12:13, 25:26, 38:39, 51:52, 64:65, 77:78, 90:91, 103:104, 116:117, 129:130, ##HIGH
                       142:143, 155:156, 168:169, 181:182, 194:195, 207:208, 220:221, 233:234, 246:247, 259:260, ##LOW
                   262)] ##SEX

##get into long format
dat_Context = melt(dat_Context, id.vars = c("id", "Sex"))

#Make the fWHR variable
dat_Context$fWHR = c(rep("High", nrow(dat_Context) / 2), rep("Low", nrow(dat_Context) / 2))

dat_Context$variable = sub('.*(?=...$)', '',
                         dat_Context$variable,
                         perl = T)

colnames(dat_Context)[3] = "Context"
colnames(dat_Context)[4] = "Score"

dat_Context$Score = as.numeric(dat_Context$Score)

####Okay, Model time!####

#note: pm = parenting motives and uses 'dat_scale' dataset
      #mi = mating interests and uses 'dat_context' dataset

##going to start w/ Parenting motives
#build the final model first
pm.final = lmer(Score ~ Sex * Motive * fWHR + (1|id),
                data = dat_scale,
                REML = F)

summary(pm.final)
Anova(pm.final)

####Break down interactions####
##SEX X MOTIVE
##break down the 2-way between parenting and fwhr
ef1 = effect(term = "Sex * Motive",  mod = pm.final)
plot(ef1)

##Try another way of visualizing
plot(ef1, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parenting Motivation as a function of Sex",
     xlab = "Sex",
     ylab = "Score")

##SEX X FWHR
ef2 = effect(term = "Sex * fWHR",  mod = pm.final)
plot(ef2)

##Try another way of visualizing
plot(ef2, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of fWHR and Sex",
     xlab = "Sex",
     ylab = "Score")

#intercept only model
pm.intercept = lmer(Score ~ (1|id),
                    data = dat_scale,
                    REML = F)

summary(pm.intercept)

#between only
pm.between = lmer(Score ~ Sex + (1|id),
                    data = dat_scale,
                    REML = F)

summary(pm.between)

#main effects only
pm.me = lmer(Score ~ Sex + Motive + fWHR + (1|id),
                  data = dat_scale,
                  REML = F)

summary(pm.me)
Anova(pm.me)

##Model comparison
anova(pm.intercept, pm.between, pm.me, pm.final) ##final model provides best fit

##Now for the Mating interest model
#build the final model
mi.final = lmer(Score ~ Sex * Context * fWHR + (1|id),
                data = dat_Context,
                REML = F)

summary(mi.final)
Anova(mi.final)

#intercept only
mi.intercept = lmer(Score ~ (1|id),
                    data = dat_Context,
                    REML = F)

#between factors
mi.between = lmer(Score ~ Sex + (1|id),
                    data = dat_Context,
                    REML = F)
#main effects
mi.me = lmer(Score ~ Sex + Context + fWHR + (1|id),
             data = dat_Context,
             REML = F)

##model comparisons
anova(mi.intercept, mi.between, mi.me, mi.final) #final model again provides the best fit

####INTERACTION -- CONTEXT X FWHR####
ef3 = effect(term = "Context * fWHR",  mod = mi.final)
plot(ef3)

##Try another way of visualizing
plot(ef3, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of fWHr and Context",
     xlab = "Context",
     ylab = "Score")
