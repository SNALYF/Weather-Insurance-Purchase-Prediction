##Clean out environment##
rm(list = ls())

##Install library##
library(readstata13)
library(Matrix)
library(gamlr)
library(dplyr)
library(vtable)
library(ranger)
library(ggplot2)
library(stargazer)
# Environment cleaning and settings ---------------------------------------
##Read Data##
if (file.exists("data/0422survey.dta")) {
  df <- read.dta13("data/0422survey.dta")
} else {
  stop("data/0422survey.dta not found in project directory.")
}

##Import ROC function for later use##
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}

##Import R^2 calculation function##
logit_dev <- function(sample_Ys, predictions) {
  return(-sum(sample_Ys * log(predictions) + (1 - sample_Ys) * log(1 - predictions)))
}


###Data Observing and cleaning###
##Data Cleaning##
head(df)
dim(df)

##Check NAs in each columns##
S <- length(df)
S <- numeric(S)
for (i in 1:length(df)){
  NumNAs <- is.na(df[i])
  S[i] <- sum(NumNAs == "TRUE")
}
S

##Clean multiple NAs column (If NA >= 1000)##
column_drop <- which(S >= 1000)
colnames(df[column_drop])
df <- df[ , -(column_drop)]

#omit NAs#
df <- na.omit(df)
dim(df)

##Rearrange row number
df <- arrange(df)

###Data factorization###
##Quick check## 
str(df)

#Drop useless data#
rm(NumNAs, i, S, column_drop)

# Data Overview -----------------------------------------------------------
##Understanding regression##
under_reg0 <- glm(df$understanding ~ df$day)
under_reg1 <- glm(df$understanding ~ df$day*as.factor(df$region))
under_reg2 <- glm(df$understanding ~ df$day*as.factor(df$region) + df$risk_averse*as.factor(df$region) + df$insurance_repay*as.factor(df$region) + df$age*as.factor(df$region) +df$educ*as.factor(df$region))

stargazer(under_reg0, under_reg1, under_reg2, type = "latex", single.row = TRUE, no.space = TRUE, column.sep.width = "0.01pt", font.size = "scriptsize", out="output/Test_understanding_reg.tex")
summary(under_reg0)
summary(under_reg1)
summary(under_reg2)
###IMPORTANT!!! Stargazer do not output long variable names, I manually adjusted it in Latex based on the summary()

df_table <- df[ , -(29:50)]
df_table <- df_table[ , -1]
table_variables <- c("day", "region", "risk_averse", "insurance_repay", "age", "educ", "understanding")
A <- numeric(length(table_variables))
for (j in 1:length(A)){
  A[j] <- which(colnames(df_table) == table_variables[j])
}
df_table <-df_table[A]

st(df_table, summ=c('mean(x)','median(x)','max(x)', 'min(x)'), summ.names = c("Mean", "Median", "Max", "Min"), group = "region", out = "latex", file="output/Test_summary_table.tex")

# Logit Model OLS analysis ------------------------------------------------
#Factorize#
df <- df[,-1]
non_factor_columns <- c("takeup_survey", "age", "agpop", "educ", "rice_inc", "ricearea_2010", "disaster_prob", "understanding", "day", "risk_averse")
factor_columns <- 1:ncol(df)
S <- length(non_factor_columns)
S <- numeric(S)
for (j in 1:length(S)){
  S[j] <- which(colnames(df) == non_factor_columns[j])
}
S <- c(S, (28:30))
S <- c(S, (35:49))
S <- S[S != 0]
factor_columns <- setdiff(factor_columns, S)
factor_columns

##Factorize variables and introduce NAs
for (i in factor_columns){
  df[, i] <- factor(df[, i])
}
str(df)
#Create another df for Random Forest before naref()#
df <- df[ , -(38:49)]
df_rf <- df

##Logit Model Analysis##
set.seed(0)
logit_reg_1 <- glm(takeup_survey ~ .- insurance_buy - village - address, data = df, family = "binomial")
significant_varaibles <- summary(logit_reg_1)$coefficients
vars <- rownames(significant_varaibles)[which(significant_varaibles[, 4] < 0.05)]
logit_reg_1$coefficients[vars]
vars
stargazer(logit_reg_1, type = "text", keep = vars, omit = vars, out="output/Test_logit_reg_1.txt")


# Lasso IS and OOS analysis -----------------------------------------------
##Lasso Analysis##

df <- naref(df)

#Testify#
levels(df$age)
levels(df$literacy)
levels(df$insurance_buy)

#QUick Check Variables again#
str(df)

##Cross-validation##
##IS CV##
set.seed(0)
x_matrix_cv_1 <- model.matrix(df$takeup_survey ~ . - address - village - insurance_buy, data = df)[ , -1]
cv.logit1 <- cv.gamlr(x_matrix_cv_1, df$takeup_survey, nfold = 30, family = "binomial")

png("output/Test_CV_Lasso_Path_IS.png")
par(mfrow=c(1,2))
plot(cv.logit1$gamlr, main = "Lasso Regularization path")
plot(cv.logit1, main = "Error against Lmabda")
dev.off()
dim(x_matrix_cv_1)

non_zero_coefs_IS <- coef(cv.logit1, select = "min")[which(coef(cv.logit1, select = "min") != 0), ]
length(non_zero_coefs_IS)
non_zero_coefs_IS
sort(non_zero_coefs_IS, decreasing = TRUE)

#predictions from cv_1#
predict_IS_cv <- predict(cv.logit1, x_matrix_cv_1, select = "min", type = "response")
actual_insurance_buy <- df$takeup_survey

#Histogram of the prediction_cv_1#
png("output/Test_CV_Pred_Hist_IS.png")
hist(predict_IS_cv, breaks = 50, xlab = "Predicted Probabilities", ylab = "Numbers of probabilities", main = "Distribution of the prediction probabilities", axes = FALSE)
axis(2)
axis(1, at = seq(0, 1, by = 0.02), labels = seq(0, 1, by = 0.02))
dev.off()

#IS ROC#
#IS ROC#
png("output/Test_ROC_IS.png")
roc(predict_IS_cv, actual_insurance_buy, bty = "n", main = "IS ROC")
cut_offs <- c(0.02, 0.1, 0.5, 0.8, 0.9)
points_colors <- c("blue", "yellow", "red", "green", "pink")
for(j in 1:length(cut_offs)){
  points(x = 1 - mean((predict_IS_cv <= cut_offs[j])[df$takeup_survey == 0]), y = mean((predict_IS_cv >cut_offs[j])[df$takeup_survey == 1]), cex = 3, pch = 20, col = points_colors[j])
}
legend("bottomright", fill = points_colors, legend = cut_offs, bty = "n", title = "cutoffs")
dev.off()

#Comparison#
IS_prediction <- ifelse(predict_IS_cv > 0.5, 1, 0)
prediction_correct_rate_IS <- ifelse(df$insurance_buy == IS_prediction, 1, 0)
length(prediction_correct_rate_IS[prediction_correct_rate_IS == 1])/length(prediction_correct_rate_IS) * 100

actual_correct_rate_IS <- ifelse(df$takeup_survey == IS_prediction, 1, 0)
actual_correct_rate_IS <- length(actual_correct_rate_IS[actual_correct_rate_IS == 1])/length(actual_correct_rate_IS) * 100
actual_correct_rate_IS

#R2 IS#
1 - cv.logit1$cvm[cv.logit1$seg.min]/cv.logit1$cvm[1]

#Out of Sample CV#
set.seed(0)
#data splitting#
par(mfrow=c(1,2))
test <- sample.int(nrow(df), nrow(df)/2)
logit_reg_oos <- cv.gamlr(x_matrix_cv_1[-test, ], df$takeup_survey[-test], family = "binomial", nfold = 30)
predict_OOS_cv <- predict(logit_reg_oos, x_matrix_cv_1[test, ], type = "response", select = "min")
Y_oos <- df$takeup_survey[test]

non_zero_coefs_OOS <- coef(logit_reg_oos, select = "min")[which(coef(cv.logit1, select = "min") != 0), ]
length(non_zero_coefs_OOS)
non_zero_coefs_OOS
sort(non_zero_coefs_OOS, decreasing = TRUE)

#Plots
#Plots
png("output/Test_CV_Lasso_Path_OOS.png")
par(mfrow=c(1,2))
plot(cv.logit1$gamlr, main = "Lasso Regularization path")
plot(cv.logit1, main = "Error against Lambda")
dev.off()
dim(x_matrix_cv_1)

#Histogram of the prediction_cv_1#
#Histogram of the prediction_cv_1#
png("output/Test_ROC_OOS.png")
hist(predict_OOS_cv, breaks = 50, xlab = "Predicted Probabilities", ylab = "Numbers of probabilities", main = "Distribution of the prediction probabilities", axes = FALSE)
axis(2)
axis(1, at = seq(0, 1, by = 0.02), labels = seq(0, 1, by = 0.02))

roc(predict_OOS_cv, Y_oos, bty = "n", main = "OOS ROC")
for(i in 1:length(cut_offs)){
  points(x = 1 - mean((predict_OOS_cv <= cut_offs[i])[Y_oos == 0]), y = mean((predict_OOS_cv >cut_offs[i])[Y_oos == 1]), cex = 3, pch = 20, col = points_colors[i])
}
legend("bottomright", fill = points_colors, legend = cut_offs, bty = "n", title = "cutoffs")
dev.off()

#comparison#
OOS_prediction <- ifelse(predict_OOS_cv > 0.5, 1, 0)
prediction_correct_rate_OOS <- ifelse(df$insurance_buy[test] == OOS_prediction, 1, 0)
length(prediction_correct_rate_OOS[prediction_correct_rate_OOS == 1])/length(prediction_correct_rate_OOS) * 100

actual_correct_rate_OOS <- ifelse(df$takeup_survey[test] == OOS_prediction, 1, 0)
actual_correct_rate_OOS <- length(actual_correct_rate_OOS[actual_correct_rate_OOS == 1])/length(actual_correct_rate_OOS) * 100
actual_correct_rate_OOS

#OOS R2#
R2_OOS_CV <- 1 - logit_reg_oos$cvm[logit_reg_oos$seg.min]/logit_reg_oos$cvm[1]
R2_OOS_CV

1 - logit_dev(df$takeup_survey[test], predict_OOS_cv)/logit_dev(df$takeup_survey[test], mean(df$takeup_survey[-test]))

# Random Forest -----------------------------------------------------------
##Random Forest##
#In-Sample Random Forest#
set.seed(0)
randomforest_reg_IS <- ranger(takeup_survey ~ ., data = df_rf, write.forest = TRUE, num.trees = 300, min.node.size = 3, importance = "impurity", probability = TRUE)
randomforest_pred_IS <- predict(randomforest_reg_IS, df_rf, type = "response")$predict
png("output/Test_RF_Importance_IS.png")
par(mfrow = c(1,2))
par(mar=c(12,4,1,1))
barplot(sort(importance(randomforest_reg_IS), decreasing = TRUE), ylab = "Frequency", las = 2, ylim = c(0, 180), main = "In-Sample Frequency")
dev.off()
randomforest_pred_organize_IS <- randomforest_pred_IS[ , 2]
randomforest_classification_IS <- ifelse(randomforest_pred_organize_IS > 0.5, 1, 0)
prediction_correct_rate_rf_IS <- ifelse(df_rf$insurance_buy == randomforest_classification_IS, 1, 0)
length(prediction_correct_rate_rf_IS[prediction_correct_rate_rf_IS == 1])/length(prediction_correct_rate_rf_IS) * 100

actual_correct_rf_IS <- ifelse(df_rf$takeup_survey == randomforest_classification_IS, 1, 0)
actual_correct_rate_rf_IS <- length(actual_correct_rf_IS[actual_correct_rf_IS == 1])/length(actual_correct_rf_IS)
actual_correct_rate_rf_IS

#IS RandomForest Prediction distribution
#IS RandomForest Prediction distribution
png("output/Test_RF_Pred_Dist_IS.png")
par(mfrow=c(1,2))
hist(randomforest_pred_organize_IS, breaks = 50, xlab = "Predicted Probabilities", ylab = "Numbers of probabilities", main = "Distribution of the prediction probabilities", axes = FALSE)
axis(2) 
axis(1, at = seq(0, 1, by = 0.02), labels = seq(0, 1, by = 0.02))

for (i in 1:length(cut_offs)){
  abline(v = cut_offs[i], col = points_colors[i], lwd=2)
}
dev.off()
#IS ROC Curve Random Forest
#IS ROC Curve Random Forest
png("output/Test_RF_ROC_IS.png")
roc(randomforest_pred_organize_IS, actual_insurance_buy, bty = "n", main = "IS Random Forest ROC")
cut_offs <- c(0.02, 0.1, 0.5, 0.8, 0.9)
points_colors <- c("blue", "yellow", "red", "green", "pink")
for(j in 1:length(cut_offs)){
  points(x = 1 - mean((randomforest_pred_organize_IS <= cut_offs[j])[df$takeup_survey == 0]), y = mean((randomforest_pred_organize_IS >cut_offs[j])[df$takeup_survey == 1]), cex = 1.5, pch = 20, col = points_colors[j])
}
legend("bottomright", fill = points_colors, legend = cut_offs, bty = "n", title = "cutoffs")
dev.off()

#R2 IS RF#
randomforest_reg_IS$r.squared

#OUt-Of-Sample Random Forest#
#Data Splitting
set.seed(0)
test <- sample.int(nrow(df_rf), nrow(df_rf)/10)

#OOS Random Forest
randomforest_reg_OOS <- ranger(takeup_survey ~ ., data = df_rf[-test, ], write.forest = TRUE, num.trees = 300, min.node.size = 3, importance = "impurity", probability = TRUE)
randomforest_pred_OOS <- predict(randomforest_reg_OOS, df_rf[test, ], type = "response")$predict
Y_OOS <- df$takeup_survey[test]

png("output/Test_RF_Importance_OOS.png")
par(mar=c(12,4,1,1))
barplot(sort(importance(randomforest_reg_OOS), decreasing = TRUE), las = 2, ylim = c(0, 150), main = "Out-of-Sample Frequency", ylab = "Frequency")
dev.off()
randomforest_pred_organize_OOS <- randomforest_pred_OOS[ , 2]
randomforest_classification_OOS <- ifelse(randomforest_pred_organize_OOS > 0.5, 1, 0)
prediction_correct_rate_rf_OOS <- ifelse(df_rf$insurance_buy[test] == randomforest_classification_OOS, 1, 0)
length(prediction_correct_rate_rf_OOS[prediction_correct_rate_rf_OOS == 1])/length(prediction_correct_rate_rf_OOS) * 100

actual_correct_rf_OOS <- ifelse(df_rf$takeup_survey[test] == randomforest_classification_OOS, 1, 0)
actual_correct_rate_rf_OOS <- length(actual_correct_rf_OOS[actual_correct_rf_OOS == 1])/length(actual_correct_rf_OOS) * 100
actual_correct_rate_rf_OOS

#R2

#OOS ROC Random Forest
roc(randomforest_pred_organize_OOS, Y_OOS, bty = "n", main = "OOS Random Forest ROC")
cut_offs <- c(0.02, 0.1, 0.5, 0.8, 0.9)
points_colors <- c("blue", "yellow", "red", "green", "pink")
for(j in 1:length(cut_offs)){
  points(x = 1 - mean((randomforest_pred_organize_OOS <= cut_offs[j])[Y_OOS == 0]), y = mean((randomforest_pred_organize_OOS >cut_offs[j])[Y_OOS == 1]), cex = 1.5, pch = 20, col = points_colors[j])
}
legend("bottomright", fill = points_colors, legend = cut_offs, bty = "n", title = "cutoffs")

#OOS RandomForest Prediction distribution
png("output/Test_RF_ROC_OOS.png")
#OOS RandomForest Prediction distribution
hist(randomforest_pred_organize_OOS, breaks = 50, xlab = "Predicted Probabilities", ylab = "Numbers of probabilities", main = "Distribution of the prediction probabilities of Out-of-Sample Randomforest", axes = FALSE)
axis(2) 
axis(1, at = seq(0, 1, by = 0.02), labels = seq(0, 1, by = 0.02))
for (i in 1:length(cut_offs)){
  abline(v = cut_offs[i], col = points_colors[i], lwd=2)
}

roc(randomforest_pred_organize_OOS, Y_OOS, bty = "n", main = "OOS Random Forest ROC")
cut_offs <- c(0.02, 0.1, 0.5, 0.8, 0.9)
points_colors <- c("blue", "yellow", "red", "green", "pink")
for(j in 1:length(cut_offs)){
  points(x = 1 - mean((randomforest_pred_organize_OOS <= cut_offs[j])[Y_OOS == 0]), y = mean((randomforest_pred_organize_OOS >cut_offs[j])[Y_OOS == 1]), cex = 1.5, pch = 20, col = points_colors[j])
}
legend("bottomright", fill = points_colors, legend = cut_offs, bty = "n", title = "cutoffs")
dev.off()
K <- 5
n <- nrow(df)
random_folds <- rep(1:K, each = ceiling(n/K))[sample(1:n)]
store_results <- data.frame(LASSO = rep(NA, K), RF = rep(NA, K))
for (k in 1:K) {
  train <- random_folds != k
  # lasso
  lin <- cv.gamlr(x = x_matrix_cv_1[train, ], y = df$takeup_survey[train], lmr = 1e-04, family = "binomial")
  yhat.lin <- drop(predict(lin, x_matrix_cv_1[!train, ], type = "response", select = "min"))
  store_results$LASSO[k] <- sqrt(sum((df$takeup_survey[!train] - yhat.lin)^2))
  # random forest
  rf <- ranger(takeup_survey ~ ., data = df_rf[train, ], num.tree = 300, min.node.size = 3, write.forest = TRUE, probability = TRUE)
  yhat.rf <- predict(rf, data = df_rf[!train, ], type = "response")$predictions[, 2]
  store_results$RF[k] <- sqrt(sum((df_rf$takeup_survey[!train] - yhat.rf)^2))
}

png("output/Test_Model_Comparison_Boxplot.png")
par(mai = c(0.8, 0.8, 0.1, 0.1))
boxplot(as.data.frame(store_results), col = "dodgerblue", xlab = "model", ylab = "root-MSE")
dev.off()


