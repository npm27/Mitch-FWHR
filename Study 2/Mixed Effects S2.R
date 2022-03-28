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
dat = as.data.set(spss.system.file("Study 2.sav"))
dat = as.data.frame(dat)

##see what we're working with
summary(dat)
length(unique(dat$id))

####Get the data in the right format####
##Get the data in long format
#going to drop the mean columns
dat.long = melt(dat[ , -c(45:52)],
                id.vars = c("id", "Sex", "Age", "Race"))

colnames(dat.long)[5:6] = c("Type", "Score")

#now split out "type" column to get high/low, N/P, I/U
#FWHR
dat.long$fwhr = dat.long$Type
dat.long$fwhr = substr(dat.long$fwhr, 0, 1)

#PARENTING
dat.long$Parenting = dat.long$Type

dat.long$Parenting = sub('.*(?=.{2}$)', '', dat.long$Parenting, perl = T)
dat.long$Parenting = substr(dat.long$Parenting, 1, 1)

#Presentation
dat.long$Type = sub('.*(?=.{2}$)', '', dat.long$Type, perl = T)
dat.long$Type = substr(dat.long$Type, 2, 2)

colnames(dat.long)[5] = "Presentation"

#make score numeric
dat.long$Score = as.numeric(dat.long$Score)

##check out the final dataset
summary(dat.long)

####Can I recreate the ANOVA?####
ezANOVA(dat.long,
        wid = id,
        between = Sex,
        within = .(Parenting, fwhr, Presentation),
        dv = Score,
        type = 3,
        detailed = T)

####Mixed effects time!####
##based on S1, going to use lmer models
#go ahead and start with final model
model.final = lmer(Score ~ Sex * Parenting * fwhr * Presentation + (1|id),
              data = dat.long,
              REML = FALSE)
summary(model.final)
Anova(model.final)

#Intercept only
model.int = lmer(Score ~  + (1|id),
                  data = dat.long,
                  REML = FALSE)
summary(model.int)

#between only
model.between = lmer(Score ~ Sex + (1|id),
                      data = dat.long,
                      REML = FALSE)
summary(model.between)

##compare models (full model should have a better fit)
anova(model.int, model.final)
anova(model.between, model.final)

##Get BF
bayestestR::bayesfactor_models(model.final, denominator = model.int)
bayestestR::bayesfactor_models(model.final, denominator = model.between)

####Breakdown the interaction(s)####
##break down the 2-way between parenting and fwhr
ef1 = effect(term = "Parenting * fwhr",  mod = model.final)
plot(ef1)

##Try another way of visualizing
plot(ef1, multiline = TRUE, confint = TRUE, ci.style = "bars",
     main = "Parenting perceptions as a function of Parenting Role and fwhr",
     xlab = "Parenting",
     ylab = "Score")
