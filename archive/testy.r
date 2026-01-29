###Environment cleaning and settings###
##Clean out environment##
rm(list = ls())

##Install library##
library(readstata13)
library(Matrix)
library(gamlr)
library(dplyr)

##Read Data##
df <- read.dta13("C:/Users/18313/Desktop/0422survey.dta")

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
df <- df[ , -(column_drop)]

#omit NAs#
df <- na.omit(df)
dim(df)

##Drop useless columns##
which(colnames(df) == "network_onlyone")
which(colnames(df) == "path_out_ind")
df <- df[ , -(37:55)]
df <- df[ , -1]

which(colnames(df) == "network_rate_preintensive")
which(colnames(df) == "network_rate_presimple")
df <- df[ , -(28:30)]


##organizing Friends variables
df$friend <- 0
x <- numeric(length(df$friend))
x[df$friend1 == 1] <- 1
x[df$friend2 == 1] <- 2
x[df$friend3 == 1] <- 3
x[df$friend4 == 1] <- 4
x[df$friend5 == 1] <- 5
df$friend <- x 


#Drop useless Friend columns#
which(colnames(df) == "friend1")
which(colnames(df) == "friend5")
df <- df[ , -(28:32)]
df <- df[ , -28]

##Rearrange row number
df <- arrange(df)



###Data factorization###
##Quick check## 
str(df)

##Factorize##
factor_columns <- 1:ncol(df)
S <- 7
numeric(S)
for (j in 1:7){
  non_factor_columns <- c("age", "rice_inc", "ricearea_2010", "disaster_prob", "understanding", "day", "risk_averse")
  S[j] <- which(colnames(df) == non_factor_columns[j])
}
factor_columns <- factor_columns[-S]
factor_columns

##Factorize variables and introduce NAs
for (i in factor_columns){
  df[, i] <- factor(df[, i])
}
df <- naref(df)

#Testify#
levels(df$age)
levels(df$literacy)


#Drop useless data#
rm(NumNAs, column_drop, i, S, j, factor_columns, non_factor_columns, x)


###Analysis###
##Logit Model Analysis##
logit_reg_1 <- glm(df$insurance_buy ~ ., data = df, family = "binomial")
summary(logit_reg_1)

##Lasso Analysis
#Cross-validation
x_matrix_cv_1 <- model.matrix(df$insurance_buy ~ ., data = df)[ , -1]
cv.logit1 <- cv.gamlr(x_matrix_cv_1, df$insurance_buy, nfold = 30, family = "binomial")
plot(cv.logit1$gamlr)
plot(cv.logit1)
