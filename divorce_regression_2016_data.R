#============================
#-------Load Functions-------
#============================
# functions from: matt hoover (mh1899@georgetown.edu)
# the mean_abs_error function takes a series of actual values (real) and
# predicted values (predict) and calculates the mean absolute error for the
# data. this will return one value for the entire data.
mean_abs_error <- function(real, predict) {
  return(mean(abs(real - predict)))
}

# the root_mean_sq_error function takes a series of actual values (real) and
# predicted values (predict) and calculates the root mean square error for the
# data. this will return one value for the entire data.
root_mean_sq_error <- function(real, predict) {
  return(sqrt(mean((real - predict)^2)))
}

# the predicted_var function takes a series of actual values (real) and
# predicted values (predict) and calculates the predicted variances explained
# for the data. this will return one value for the entire data.
predicted_var <- function(real, predict) {
  return(1 - (var(real) - var(predict)) / var(real))
}


#============================
#----------Load Data---------
#============================
#import data from CSV file
my_data <- read.csv('final_training.csv')
test_data <- read.csv('final_test.csv')
na_data <- read.csv('final_na.csv')

#load descriptive statistics package
#install.packages("psych")
library(psych)

#check data was imported
head(my_data)


#------Summary------
summary(my_data$divorce_rate)
describe(my_data$divorce_rate)
boxplot(my_data$divorce_rate, data = my_data)


#------Comparison------
plot(my_data)


#================================================
#---create data sets to see plots more clearly---
#================================================

#---------------------------
#---------education---------
#---------------------------
edu_data <- data.frame(my_data$divorce_rate, my_data$percent_hs_grad, my_data$percent_bach_grad, my_data$percent_adv_grad)
names(edu_data) <- c("divorce_rate", "percent_hs_grad", "percent_bach_grad", "percent_adv_grad")
plot(edu_data)
edu_reg <- lm(divorce_rate ~ percent_hs_grad + percent_bach_grad + percent_adv_grad, data = edu_data)
summary(edu_reg)
#percent_bach_grad looks best out of the education variables



#---------------------------
#---------marriage----------
#---------------------------
marriage_data <- data.frame(my_data$divorce_rate, my_data$cost, my_data$num_weddings, my_data$avg_hh_inc_wed, my_data$avg_ring, my_data$marriage_rate, my_data$age_marriage_f, my_data$age_marriage_m, my_data$age_marriage_diff, my_data$hh_with_kids, my_data$percent_single_hh)
names(marriage_data) <- c("divorce_rate", "cost", "num_weddings", "avg_hh_inc_wed", "avg_ring", "marriage_rate", "age_marriage_f", "age_marriage_m", "age_marriage_diff", "hh_with_kids", "percent_single_hh")
plot(marriage_data)
marriage_reg <- lm(divorce_rate ~ cost + num_weddings + avg_hh_inc_wed + avg_ring + marriage_rate + age_marriage_f + age_marriage_m + age_marriage_diff + hh_with_kids + percent_single_hh, data = marriage_data)
summary(marriage_reg)
#cost, avg_hh_inc_wed, avg_ring, marriage_rate, age_marriage_f, hh_with_kids - we can narrow down below



#---------------------------
#------marriage subset------
#---------------------------
marriage_data_mini <- data.frame(my_data$divorce_rate, my_data$cost, my_data$avg_hh_inc_wed, my_data$avg_ring, my_data$marriage_rate, my_data$age_marriage_f, my_data$hh_with_kids)
names(marriage_data_mini) <- c("divorce_rate", "cost", "avg_hh_inc_wed", "avg_ring", "marriage_rate", "age_marriage_f", "hh_with_kids")
plot(marriage_data_mini)
mar_reg_mini <- lm(divorce_rate ~ cost + avg_hh_inc_wed + avg_ring + marriage_rate + age_marriage_f + hh_with_kids, data = marriage_data_mini)
summary(mar_reg_mini)
#marriage_rate, age_marriage_f, and hh_with_kids are most significant



#---------------------------
#------everything else------
#---------------------------
other_data <- data.frame(my_data$divorce_rate, my_data$med_inc, my_data$birth_rate, my_data$unemployment, my_data$pol_dem, my_data$pol_rep, my_data$pop_over_18, my_data$percent_sun, my_data$clear_days, my_data$hours_sun)
names(other_data) <- c("divorce_rate", "med_inc", "birth_rate", "unemployment", "pol_dem", "pol_rep", "pop_over_18", "percent_sun", "clear_days", "hours_sun")
plot(other_data)
other_reg <- lm(divorce_rate ~ med_inc + birth_rate + unemployment + pol_dem + pol_rep + pop_over_18 + percent_sun + clear_days + hours_sun, data = other_data)
summary(other_reg)
#birth_rate, percent_sun


#---------------------------
#---everything else subset--
#---------------------------
other_data <- data.frame(my_data$divorce_rate, my_data$med_inc, my_data$birth_rate, my_data$unemployment, my_data$pol_dem, my_data$pol_rep, my_data$pop_over_18, my_data$percent_sun)
names(other_data) <- c("divorce_rate", "med_inc", "birth_rate", "unemployment", "pol_dem", "pol_rep", "pop_over_18", "percent_sun")
plot(other_data)
other_reg <- lm(divorce_rate ~ med_inc + birth_rate + unemployment + pol_dem + pol_rep + pop_over_18 + percent_sun, data = other_data)
summary(other_reg)
#birth_rate, unemployment, percent_sun

#============================
#----Univariate Statistics---
#============================
#we look at the summary and descriptive statistics of the varaibles we've selected
summary(my_data$percent_bach_grad)
describe(my_data$percent_bach_grad)

summary(my_data$marriage_rate)
describe(my_data$marriage_rate)

summary(my_data$age_marriage_f)
describe(my_data$age_marriage_f)

summary(my_data$hh_with_kids)
describe(my_data$hh_with_kids)

summary(my_data$unemployment)
describe(my_data$unemployment)

summary(my_data$birth_rate)
describe(my_data$birth_rate)

summary(my_data$percent_sun)
describe(my_data$percent_sun)


#============================
#--------REGRESSION 1--------
#============================
#contains percent_bach_grad, marriage_rate, age_marriage_f, hh_with_kids, birth_rate, unemployment, percent_sun

reg_data1 <- data.frame(my_data$divorce_rate, my_data$percent_bach_grad, my_data$marriage_rate, my_data$age_marriage_f, my_data$hh_with_kids, my_data$unemployment, my_data$percent_sun, my_data$birth_rate)
names(reg_data1) <- c("divorce_rate", "percent_bach_grad", "marriage_rate", "age_marriage_f", "hh_with_kids", "unemployment", "percent_sun", "birth_rate")
plot(reg_data1)

reg1 <- lm(divorce_rate ~ percent_bach_grad + marriage_rate + age_marriage_f + hh_with_kids + unemployment + percent_sun + birth_rate, data = reg_data1)
summary(reg1)

reg1_predict <- predict(reg1, reg_data1)
reg1_predict

#============================
#--------REGRESSION 2--------
#============================
#contains percent_bach_grad, marriage_rate, age_marriage_f, hh_with_kids, unemployment

reg_data2 <- data.frame(my_data$divorce_rate, my_data$percent_bach_grad, my_data$marriage_rate, my_data$age_marriage_f, my_data$hh_with_kids, my_data$unemployment)
names(reg_data2) <- c("divorce_rate", "percent_bach_grad", "marriage_rate", "age_marriage_f", "hh_with_kids", "unemployment")
plot(reg_data2)

reg2 <- lm(divorce_rate ~ percent_bach_grad + marriage_rate + age_marriage_f + hh_with_kids + unemployment, data = reg_data1)
summary(reg2)

reg2_predict <- predict(reg2, reg_data2)
reg2_predict


#============================
#--------REGRESSION 3--------
#============================
#contains marriage_rate, age_marriage_f, hh_with_kids, unemployment

reg_data3 <- data.frame(my_data$divorce_rate, my_data$marriage_rate, my_data$age_marriage_f, my_data$hh_with_kids, my_data$unemployment)
names(reg_data3) <- c("divorce_rate", "marriage_rate", "age_marriage_f", "hh_with_kids", "unemployment")
plot(reg_data3)

reg3 <- lm(divorce_rate ~ marriage_rate + age_marriage_f + hh_with_kids + unemployment, data = reg_data1)
summary(reg3)

reg3_predict <- predict(reg3, reg_data3)
reg3_predict




#============================
#---------R Squared----------
#============================
cbind(summary(reg1)$r.squared,
      summary(reg2)$r.squared,
      summary(reg3)$r.squared)


#============================
#-----------AIC/BIC----------
#============================
rbind(
  cbind(AIC(reg1),
        AIC(reg2),
        AIC(reg3)),
  cbind(BIC(reg1),
        BIC(reg2),
        BIC(reg3))
)



#============================
#----Prediction Metrics------
#============================
# for training data
rbind(
# root mean square error
cbind(root_mean_sq_error(my_data$divorce_rate, predict(reg1, my_data)),
      root_mean_sq_error(my_data$divorce_rate, predict(reg2, my_data)),
      root_mean_sq_error(my_data$divorce_rate, predict(reg3, my_data))),

# mean absolute error
cbind(mean_abs_error(my_data$divorce_rate, predict(reg1, my_data)),
      mean_abs_error(my_data$divorce_rate, predict(reg2, my_data)),
      mean_abs_error(my_data$divorce_rate, predict(reg3, my_data))),

# predicted variance
cbind(predicted_var(my_data$divorce_rate, predict(reg1, my_data)),
      predicted_var(my_data$divorce_rate, predict(reg2, my_data)),
      predicted_var(my_data$divorce_rate, predict(reg3, my_data))))


#============================
#----Prediction Metrics------
#============================
# for test data
rbind(
# root mean square error
cbind(root_mean_sq_error(my_data$divorce_rate, predict(reg1, test_data)),
      root_mean_sq_error(my_data$divorce_rate, predict(reg2, test_data)),
      root_mean_sq_error(my_data$divorce_rate, predict(reg3, test_data))),

# mean absolute error
cbind(mean_abs_error(my_data$divorce_rate, predict(reg1, test_data)),
      mean_abs_error(my_data$divorce_rate, predict(reg2, test_data)),
      mean_abs_error(my_data$divorce_rate, predict(reg3, test_data))),

# predicted variance
cbind(predicted_var(my_data$divorce_rate, predict(reg1, test_data)),
      predicted_var(my_data$divorce_rate, predict(reg2, test_data)),
      predicted_var(my_data$divorce_rate, predict(reg3, test_data))))



#============================
#----Prediction--------------
#============================
# for NA data
r1_predictions <- predict(reg1, na_data)
r2_predictions <- predict(reg2, na_data)
r3_predictions <- predict(reg3, na_data)

predictions <- data.frame(state = na_data$State,
                          r1 = r1_predictions,
                          r2 = r2_predictions,
                          r3 = r3_predictions)

predictions