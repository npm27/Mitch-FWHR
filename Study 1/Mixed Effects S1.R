#Study1
#2 x 2 repeated design
####Set up####
##Load libraries
library(memisc) #read in .sav
library(reshape) #data wrangling
library(lme4) #Mixed effects models
library(nlme) #more mixed effects models
library(ez) #anovas

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

#now split out "type" column to get high/low and N/P
dat.long$fwhr = dat.long$Type
dat.long$fwhr = substr(dat.long$fwhr, 0, 1)

dat.long$Type = sub('.*(?=.{2}$)', '', dat.long$Type, perl=T)
dat.long$Type = substr(dat.long$Type, 2, 2)

colnames(dat.long)[5] = "Parenting"

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
Final_model = lme(Score ~ Sex * Parenting * fwhr, 
                  data = dat.long,
                  method = "ML", 
                  na.action = "na.omit",
                  random = ~1|id)
summary(Final_model)
