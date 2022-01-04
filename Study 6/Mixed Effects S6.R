####Set up####
##load libraries
library(memisc)
library(reshape)
library(ez)
library(lme4)
library(car)

##turn of scientific notation
options(scipen = 999)

##load data
dat = as.data.set(spss.system.file("Study 6.sav"))
dat = as.data.frame(dat)

##make columns for the different variables
#need high and low for fwhr. Also need both pcat subscales (p and w?)

###fwhr
##build the fwhr data
fwhr_dat = dat[ , c(1, 3:22, 33, 39, 40)]

#melt
fwhr_long = melt(fwhr_dat,
                 id.vars = c("id", "Sex",
                             "Nurture_AVG", "Protect_AVG"))

#fix column names
colnames(fwhr_long)[5:6] = c("fwhr", "score")

#fix fwhr column (just make it H's and L's)
fwhr_long$fwhr = substr(fwhr_long$fwhr, start = 1, stop = 1)

#make score numeric
fwhr_long$score = as.numeric(fwhr_long$score)

####Model time####
##Make the final model first
model.final = lmer(score ~ Sex * fwhr + Nurture_AVG + Protect_AVG + (1|id),
                   data = fwhr_long,
                   REML = FALSE)

summary(model.final)
Anova(model.final) #looks like nothing is significant
