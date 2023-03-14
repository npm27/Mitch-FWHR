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
dat = as.data.set(spss.system.file("Study 4.sav"))
dat = as.data.frame(dat)

####Get the dataset in the correct shape####
dat = melt(dat[ , -c(3, 45:50)], id.vars = c("id", "Sex", "Condition"))

##make label columns for fwhr and parenting role
#fwhr
dat$fwhr = dat$variable
dat$fwhr = substr(dat$fwhr, 0, 1)

#parenting
dat$variable = sub('.*(?=.{1}$)', '', dat$variable, perl = T)
colnames(dat)[4] = "Parenting"

#make the score column
colnames(dat)[5] = "Score"
dat$Score = as.numeric(dat$Score)

library(ez)

dat2 = na.omit(dat)

ezANOVA(dat2,
        dv = Score,
        wid = id,
        between = .(Sex, Condition),
        within = .(Parenting, fwhr),
        type = 3,
        detailed = T)

####Run the models!####
##Make the final model first
model.final = lmer(Score ~ Sex * Parenting * fwhr * Condition + (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.final)
Anova(model.final, type = "III")

#int only model
model.int = lmer(Score ~  (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.int)

#between only
model.between = lmer(Score ~ Sex + Condition + (1|id),
                   data = dat,
                   REML = FALSE)
summary(model.between)
Anova(model.between)

##main effects only
model.mf = lmer(Score ~ Sex + Parenting + fwhr + Condition + (1|id),
                     data = dat,
                     REML = FALSE)
summary(model.mf)
Anova(model.mf)

##compare models
anova(model.int, model.between, model.mf, model.final)
anova(model.int, model.final)
anova(model.mf, model.final)

##Get BF
bayestestR::bayesfactor_models(model.final, denominator = model.int)
bayestestR::bayesfactor_models(model.final, denominator = model.mf)

####Interactions####
##parenting X FWHR
ef1 = effect(term = "Parenting * fwhr",  mod = model.final)
plot(ef1)

##Try another way of visualizing
plot(ef1, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of Parenting Style and fWHR",
     xlab = "Context",
     ylab = "Score")

##FWHR X CONDITION
ef2 = effect(term = "fwhr * Condition",  mod = model.final)
plot(ef2)

##Try another way of visualizing
plot(ef2, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of Race and fWHR",
     xlab = "Context",
     ylab = "Score")

##PARETNING X FWHR X CONDITION
ef3 = effect(term = "Parenting * fwhr * Condition",  mod = model.final)
plot(ef3)

plot(ef3, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parental Effectiveness as a function of Race and fWHR",
     xlab = "Context",
     ylab = "Score")
