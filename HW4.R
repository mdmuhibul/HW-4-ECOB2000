#Md Muhibul Islam
ECOB2000
#September 23, 2022
#Suguru Iwashiro, Amira Elmakawy

#According to our research, combination of OWNCOST + RENT, HHINCOME + COSTELEC and K = 1 is highest accuracy 78.9%. Second highest is combination of HHINCOME and OWNCOST + RENT and K = 1. Accuracy is 73.04%


#This is from Lab 3 (OWNCOST + RENT, INCTOT)
1.0.3540087
3.0.3437859
5.0.3550425
7.0.3708936
9.0.3721571


#we added "HHINCOME" into the data set. (OWNCOST + RENT, INCTOT + HHINCOME)
1.0.3805268
3.0.3701585
5.0.3794542
7.0.3850554
9.0.3895841


#After that, we added "COSTELEC" into the data set.This is the highest. (OWNCOST + RENT, HHINCOME + COSTELEC)
1 0.788583
3.0.4919557
5.0.4749136
7.0.4606126
9.0.4516744


#Combination of HHINCOME and OWNCOST + RENT is also good result.(HHINCOME and OWNCOST + RENT)
1.0.7304255
3.0.4983911
5.0.4797998
7.0.4675247
9.0.4659754


library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(dplyr)
library(tidyr)
library(psych)
options(dplyr.summarise.inform = FALSE)


setwd("/Users/Suguru/Desktop/Econometrics")
load("acs2017_ny/acs2017_ny_data.RData")
acs2017_ny <- as.data.frame(acs2017_ny)
rm(list = ls(all = TRUE))
colnames(acs2017_ny)
attach(acs2017_ny)


dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))


norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}


is.na(OWNCOST) <- which(OWNCOST == 9999999) # that's how data codes NA values
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
norm_household <- norm_varb(HHINCOME)
norm_total_family_inc <- norm_varb(FTOTINC)
norm_elec <- norm_varb(COSTELEC)
norm_gas <- norm_varb(COSTGAS)


norm_inc_tot <- as.numeric(norm_inc_tot)
norm_housing_cost <- as.numeric(norm_housing_cost)
norm_household <- as.numeric(norm_household)
norm_total_family_inc <- as.numeric(norm_total_family_inc)
norm_elec <- as.numeric(norm_elec)
norm_gas <- as.numeric(norm_gas)


data_use_prelim <- data.frame(norm_household, norm_housing_cost,norm_elec)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)


set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]


summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)


install.packages('class')
library(class)
require(class)

for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
