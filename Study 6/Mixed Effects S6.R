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

####Get the dataset in the correct shape####
dat = melt(dat[ , -c(2, 34:42)], id.vars = c("id", "Sex"))

##make columns for the different variables