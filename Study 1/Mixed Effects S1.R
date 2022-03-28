#Study1
#2 x 2 repeated design
####Set up####
##Load libraries
library(memisc) #read in .sav
library(reshape) #data wrangling
library(lme4) #Mixed effects models
library(nlme) #more mixed effects models
library(ez) #anovas
library(car) #get p-values
library(effects) #maybe use this for interactions?
library(ggplot2) #plot the things!
library(emmeans)

##turn of scientific notation
options(scipen = 999)

##Read in the dataset
dat = as.data.set(spss.system.file("Study1.sav"))
dat = as.data.frame(dat)

##Check the data
summary(dat)
length(unique(dat$id))

#fix column names
colnames(dat)[2] = "High1N"
colnames(dat)[3] = "High1P"

#which ones are female?
table(dat$Sex) #2's

#let's factor sex
dat$Sex = factor(dat$Sex,
                 levels = c(1, 2),
                 labels = c("Male", "Female"))

##Get the data in long format
#going to drop the mean columns
dat.long = melt(dat[ , -c(45:48)],
                id.vars = c("id", "Sex", "Age", "Race"))

colnames(dat.long)[5:6] = c("Type", "Score")

##now split out "type" column to get high/low and N/P
#fwhr
dat.long$fwhr = dat.long$Type
dat.long$fwhr = substr(dat.long$fwhr, 0, 1)

#parenting
dat.long$Type = sub('.*(?=.{2}$)', '', dat.long$Type, perl=T)
dat.long$Type = substr(dat.long$Type, 2, 2)
colnames(dat.long)[5] = "Parenting"

#make score numeric
dat.long$Score = as.numeric(dat.long$Score)

####Analyses####
##first try to reproduce the means
tapply(dat.long$Score, list(dat.long$Parenting, dat.long$fwhr), mean, na.rm = T)

#Remove the one NA
dat.long = na.omit(dat.long)

##Now the anova
ezANOVA(dat.long,
        wid = id,
        between = Sex,
        within = .(Parenting, fwhr),
        dv = Score,
        type = 3,
        detailed = T)

####model time!####
##Try LME model first
model1 = lme(Score ~ Sex * Parenting * fwhr, 
             data = dat.long,
             method = "ML", 
             na.action = "na.omit",
             random = ~1|id)
summary(model1)
Anova(model1) #big A gets you p-values

#Try lmer
model2 = lmer(Score ~ Sex * Parenting * fwhr + (1|id),
           data = dat.long,
           REML = FALSE)
summary(model2)
Anova(model2, type = "III")


##Okay, I like the lmer model better syntax wise, but both return the same output. I will take that as a good sign.
#first, fit an intercept only model
model2.int = lmer(Score ~  + (1|id),
                  data = dat.long,
                  REML = FALSE)
summary(model2.int)


#now add the effects
#add the between first
model2.between = lmer(Score ~ Sex + (1|id),
                  data = dat.long,
                  REML = FALSE)
summary(model2.between)

##compare models (full model should have a better fit)
anova(model2.int, model2.between, model2) #notice the little a!
#AIC and BIC both decrease for our final model (model2) meaning that it provides the best fit to our data

lmtest::lrtest(model2.between, model2)
lmtest::lrtest(model2.int, model2)

options(scipen = 999)

bayestestR::bayesfactor_models(model2, denominator = model2.between)
bayestestR::bayesfactor_models(model2, denominator = model2.int)