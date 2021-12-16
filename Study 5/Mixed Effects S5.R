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
dat = as.data.set(spss.system.file("Study 5.sav"))
dat = as.data.frame(dat)

####Get the dataset in the correct shape####
dat = melt(dat[ , -c(43:57)], id.vars = c("id", "Sex"))

#fwhr
dat$fwhr = dat$variable
dat$fwhr = substr(dat$fwhr, 0, 1)

#target sex
dat$Target_Sex = sub('.*(?=.{1}$)', '', dat$variable, perl = T)

##parenting
#parenting
dat$variable = sub('.*(?=.{2}$)', '', dat$variable, perl = T)
dat$variable = substr(dat$variable, 1, 1)

colnames(dat)[3] = "Parenting"

##score
colnames(dat)[4] = "Score"
dat$Score = as.numeric(dat$Score)

####Model Time!####
##Make the final model first
model.final = lmer(Score ~ Sex * Parenting * fwhr * Target_Sex + (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.final)
Anova(model.final)

##intercept only
model.int = lmer(Score ~ (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.int)

#between only
model.between = lmer(Score ~ Sex + Target_Sex + (1|id),
                 data = dat,
                 REML = FALSE)
summary(model.between)

#main effects only
model.mf = lmer(Score ~ Sex + Parenting + fwhr + Target_Sex + (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.mf)

##compare models
anova(model.int, model.between, model.mf, model.final)

####Interactions####
##Parenting x fwhr
ef1 = effect(term = "Parenting * fwhr",  mod = model.final)
plot(ef1)

##Try another way of visualizing
plot(ef1, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of Parenting Style and fWHR",
     xlab = "Context",
     ylab = "Score")

##Parenting X Target Sex
ef2 = effect(term = "Parenting * Target_Sex",  mod = model.final)
plot(ef2)

##Try another way of visualizing
plot(ef2, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of Parenting Style and Target Sex",
     xlab = "Context",
     ylab = "Score")
