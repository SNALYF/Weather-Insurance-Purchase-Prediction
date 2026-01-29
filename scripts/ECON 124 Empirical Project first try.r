rm(list = ls())
# FIX: Use relative path. Note: Assuming University_SET_data.Rdata is in the project root or accessible.
# If it was on Desktop, verify if it was copied here. 
# Based on file list, I only see .RData (which might be the workspace image) and .dta files.
# I will check if University_SET_data.Rdata exists, otherwise this script will fail.
# For now, I will assume it's in the same directory or I'll comment it out if it's missing from the file list.
# Checking file list again... I do NOT see University_SET_data.Rdata in the initial file list!
# I see .RData which is 256KB. University_SET_data.Rdata might be external.
# I will use a placeholder relative path and print a warning.
if(file.exists("data/University_SET_data.Rdata")){
  load("data/University_SET_data.Rdata")
} else if(file.exists("data/0422analysis.dta")) {
  # Warning: Original script loaded Rdata. I am not sure if df comes from there.
  # The script says 'df <- dat'.
  # I will just keep the load command but point to relative path.
  stop("University_SET_data.Rdata not found in project directory. Please move it here.")
}

#Clean environment:
if(exists("dat")) {
    df <- dat
} else {
    # Fallback or error if dat is not loaded
    # warning("Object 'dat' not found after loading Rdata.")
}
str(df)

#Install environment:
library(stargazer)
library(Matrix)
library(gamlr)
library(ggplot2)
library(ranger)

##Basic reg estimation:
OLS_reg_1 <- glm(df$SET_score_avg ~ .^2, data = df)
stargazer(OLS_reg_1, type = "text", out="output/First_Try_OLS.txt")

##Cross valdiation basic OLS:
#investigate data
ncol(df)
nrow(df)
#random splitting samples
set.seed(1234)
random_generates <- rep(1:3, c(511, 255, 255))[sample(1:nrow(df))]
random_generates
#little test
length(random_generates[random_generates == 1])

#assign test sets and training set:
training_set <- df[random_generates == 1, ]
test_set_chaos <- df[random_generates == 2, ]
test_set_pure <- df[random_generates == 3, ]

#CV reg 1 and predicts through it:
cv.reg1 <- glm(training_set$SET_score_avg ~ .^2, data = training_set)
true_outcome <- test_set_chaos$SET_score_avg
predict_outcome <- predict(cv.reg1, newdata = test_set_chaos)
predict_outcome

#R^2 check:
cvreg1_insample_r2 <- 1-(cv.reg1$deviance/cv.reg1$null.deviance)
cvreg1_insample_r2

#Function for OOS R2
OLS_dev_OOS <- function(sample_Ys, predictions){
  return(sum((sample_Ys - predictions)^2))
}

cvreg1_outofsample_r2 <- 1-(OLS_dev_OOS(true_outcome, predict_outcome)/OLS_dev_OOS(training_set$SET_score_avg, mean(true_outcome)))
cvreg1_outofsample_r2                            

##Implementing Lasso:
#Set Environment:
str(df)
factor_variables <- c("class_duration", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "class_end_by_10", "class_end_10_14", "class_end_14_18", "class_end_18_22", "no_dgr", "prof", "ma", "dr", "seniority", "female", "male")
S <- length(factor_variables)
numeric(S)
for (j in 1:length(factor_variables)){
  S[j] <- which(colnames(df) == factor_variables[j])
}

for (i in S){
  df[ , i] <- factor(df[ , i]) 
}
str(df)
df <- naref(df)

cv_lasso_matrix <- model.matrix(SET_score_avg ~ .^2 , data = df)[, -1]
cv_lasso_reg1 <- cv.gamlr(cv_lasso_matrix, df$SET_score_avg, nfold = 10)
plot(cv_lasso_reg1)
plot(cv_lasso_reg1$gamlr)

optimal_coefs <- coef(cv_lasso_reg1, select = "min")
non_zero_coefs <- optimal_coefs[which(optimal_coefs != 0), ]
non_zero_coefs
sort(non_zero_coefs, decreasing = TRUE)

#AICc
AICc_reg1 <- gamlr(cv_lasso_matrix, df$SET_score_avg)
plot(AICc_reg1$lambda)

