#Importing Libraries

library('readxl')
library("dplyr")
library("ggpubr")
library('caret')
library('corrplot')
library('car')
library('olsrr')
library('MASS')



# Reading Data from csv File
# Removing the Unwanted Categorical Features from Dataset

data_original <- read.csv("../input/data.csv")
#data_original[c(1:5),]
cols.dont.want <- c("X","ID","Name", "Photo", "Nationality", "Flag", "Potential", "Club", "Club.Logo", "Preferred.Foot", "Work.Rate", "Body.Type", "Real.Face", "Position", "Jersey.Number", "Joined", "Loaned.From", "Contract.Valid.Until" ) 
data <- data_original[, ! names(data_original) %in% cols.dont.want, drop = F]
#data[c(1:5),]




# Converting all Values to numeric type
# Removing all '€', 'M', 'K','ft inches', 'lbs', 'error ranges' from data
# Replacing All NA/Missing Values with Mean of that column

data$Value <- gsub("€", "", data$Value)
data$Value <- as.numeric(gsub("M", "000", data$Value))
data$Release.Clause <- gsub("€", "", data$Release.Clause)
data$Release.Clause <- as.numeric(gsub("M", "000", data$Release.Clause))
data$Wage <- gsub("€", "", data$Wage)
data$Wage <- as.numeric(gsub("K", "", data$Wage))
data$Height <- as.numeric(gsub("\'", ".", data$Height))
data$Weight <- as.numeric(gsub("lbs", "", data$Weight))
for (i in c(11:38))
  data[,i]<- as.numeric(gsub("+", ".", data[,i],fixed=TRUE))
for(i in 1:ncol(data))
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)





# Normalizing the columns of the dataset to range [0,1] so that all features get equal weightage in model.
# Normalizing to (0,1] instead of standardizing to [-1,1] because BoxCox transformation requires y to be strictly positive.


#Uncomment to standardize to [-1,1]
#data_scaled <- as.data.frame(scale(data))
#data_scaled
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(data, method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
data_scaled <- predict(preprocessParams, data) + c(0.0000001)
# summarize the transformed dataset
for(i in 1:ncol(data))
  data_scaled[is.na(data_scaled[,i]), i] <- mean(data_scaled[,i], na.rm = TRUE)
  data_scaled[is.infinite(data_scaled[,i]), i] <- mean(data_scaled[,i], na.rm = TRUE)
#data_scaled
#summary(data_scaled)





# Shuffle Rows of DataSet
# Seperate the train set and test set as 70% and 30% of given dataset

data_shuffled <- data_scaled[sample(nrow(data_scaled)),]
r1 <- as.integer(nrow(data_shuffled)*0.7) #row number at which splitting takes place
data_train <- data_shuffled[1:r1,]
data_test <- data_shuffled[(r1+1):nrow(data_shuffled),]
data_shuffled[c(1:2),]
data_train[c(1:2),]
data_test[c(1:2),]



# Separating the Dependent and Independent Variables from Training Data
# Creating Variables x_train, y_train, x_test, y_test

not_training_features <- c("Overall") 
x_train <- as.matrix(data_train[, ! names(data_train) %in% not_training_features, drop = F])
y_train <- as.matrix(data_train[,"Overall", drop = F])
x_test <- as.matrix(data_test[, ! names(data_test) %in% not_training_features, drop = F])
y_test <- as.matrix(data_test[,"Overall", drop = F])
print("Dimensions :")
print("x_train : ")
print(dim(x_train))
print("x_test : ")
print(dim(x_test))
print("y_train : ")
print(dim(y_train))
print("y_test : ")
print(dim(y_test))



#Fitting basic Multiple Linear Regression Model

lin_model1<- Overall~Age+Value+Wage+Special+International.Reputation+Weak.Foot+Skill.Moves+Height+Weight+LS+ST+RS+LW+LF+CF+RF+RW+LAM+CAM+RAM+LM+LCM+CM+RCM+RM+LWB+LDM+CDM+RDM+RWB+LB+LCB+CB+RCB+RB+Crossing+Finishing+HeadingAccuracy+ShortPassing+Volleys+Dribbling+Curve+FKAccuracy+LongPassing+BallControl+Acceleration+SprintSpeed+Agility+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+Aggression+Interceptions+Positioning+Vision+Penalties+Composure+Marking+StandingTackle+SlidingTackle+GKDiving+GKHandling+GKKicking+GKPositioning+GKReflexes+Release.Clause
fit1<-lm(lin_model1,data_train)
summary(fit1)
print("Number of Parameters Learnt = ")
print(length(fit1$coefficients))




# In the above linear model, we see - 
# 1. 16 Coefficients could not be obtained due to singularity of (X.T X) due to multicollinearity of regressors.
# 2. As p-value of Global Hypothesis is << 0.05, we reject Null hypothesis of Global Hypothesis test. Regressors are significant.
# 3. As global null hypothesis is rejected, we see individual significance test for regressors. The regressors which have p-value < 0.05 are significant, we will include those in our model. As Global hypothesis does not test intercept, we include Intercept irrespective of p-value.

# First we find the Collinear Regressors, using the Correlation Matrix.


corr_x_train <- cor(x_train)
dim(corr_x_train)
name_col_rem <- findCorrelation(corr_x_train, cutoff = 0.95, verbose = FALSE, names = TRUE, exact = TRUE)
print("Regressors to Remove: ")
name_col_rem


# Got the names of regressors which should be removed, as they have >=0.95 correlation with other regressors.
# Removing those regressors.


cols.dont.want <- name_col_rem
data_train <- data_train[, ! names(data_train) %in% cols.dont.want, drop = F]
data_test <- data_test[, ! names(data_test) %in% cols.dont.want, drop = F]
dim(data_train)
dim(data_test)
not_training_features <- c("Overall") 
x_train <- as.matrix(data_train[, ! names(data_train) %in% not_training_features, drop = F])
y_train <- as.matrix(data_train[,"Overall", drop = F])
x_test <- as.matrix(data_test[, ! names(data_test) %in% not_training_features, drop = F])
y_test <- as.matrix(data_test[,"Overall", drop = F])





# Number of regressors is reduced to 44.
# Fitting the Linear Model Again.

new_colnames <- colnames(x_train)
print("New Column Names")
new_colnames
model2_formula <- Overall~Age+Value+Wage+Special+International.Reputation+Weak.Foot+Skill.Moves+Height+Weight+RCM+LWB+LCB+Crossing+Finishing+HeadingAccuracy+ShortPassing+Volleys+Dribbling+Curve+FKAccuracy+LongPassing+BallControl+Acceleration+SprintSpeed+Agility+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+Aggression+Interceptions+Positioning+Vision+Penalties+Composure+Marking+StandingTackle+GKPositioning+Release.Clause
lin_model2<-model2_formula
fit2<-lm(lin_model2,data_train)
summary(fit2)
print("Number of Parameters Learnt = ")
print(length(fit2$coefficients))






# Individual Hypothesis Testing: 

#   We will now consider only those parameters which have a p-vale<0.05, others are insignificant
  
#   Then we fit linear model to selected regressors.


#storing True/False for Inclusion/Exclusion of all regressors based on p-value < 0.05
toselect.x <- summary(fit2)$coeff[-1,4] < 0.05
#storing names of regressors that have p-value < 0.05
relevant.x <- names(toselect.x)[toselect.x == TRUE] 

#linear model formula with only significant variables
sig.formula <- as.formula(paste("Overall ~",paste(relevant.x, collapse= "+")))
print("New Formula ")
print(sig.formula)

lin_model3 <- sig.formula
fit3<-lm(sig.formula,data_train)
summary(fit3)
print("Number of Parameters Learnt = ")
print(length(fit3$coefficients))








# See that -
#   This Reduced number of parameters learnt (= p+1) for (b0, b1, b2, ... , bp) to 36 from initial value of 71, yet Adjusted R^2 is almost equal.
      
# Graphical Analysis of Residuals - 

# Plotting Residuals vs Y (e_i vs y_i), and QQ plot to see if Variance is stabilized

plot(fit3)



# Observations - 

# 1. Residuals vs Fitted :
#     Most Residuals are equally spread along the horizontal line, but  change as the Fitted Values are very high. (Outliers)
# 2. Normal QQ Plot :
#     Data is normally distributed except few outliers.
# 3. Scale Location Plot: 
#     Data is evenly spread except for outliers when fitted values are very high
# 4. Residuals vs Leverage :
#     Cook's Distance Lines are not seen in plot, this means all cases are well inside Cook's distance lines. There are no outliers that alone are extremely influential to the regression results. 


# Density Plot of Residuals looks like it has a Normal Distribution :

plot(density(resid(fit2)))





# Applying Shapiro-Wilk's Normalcy Test :
#   H_0 : Sample is Normal, H_a: Sample not normal


shapiro.test(resid(fit2)[1:5000])
shapiro.test(resid(fit2)[5001:10000])
shapiro.test(resid(fit2)[10001:length(resid(fit2))])




# As p-value is < 0.05 for our data, Shapiro-Wilk test says that Residuals are not normally distributed.
# But Shapiro-Wilk test should be applied only for n<50, as it generally always rejects Large Datasets, even if they lie close to Normal Distribution. 

# We simulate Normal Distribution with Slight Deviation, and show that Shapiro-Wilk Test rejects 50% of all n=5000 size datasets.


x <- replicate(1000, { # generates 100 different tests on each distribution
                     c(shapiro.test(rnorm(1000)+c(0,0,1,0,0))$p.value, #$
                       shapiro.test(rnorm(5000)+c(0,0,1,0,0))$p.value) #$
                    } # rnorm gives a random draw from the normal distribution
               )
rownames(x) <- c("n1000","n5000")
rowMeans(x<0.05)





# For large datasets, Normalcy Tests are not significant because of Central Limit Theorem.

# We use Box-Cox Transformation to stabilize Variance.

model4_formula <- as.formula(paste("data_train$Overall ~",paste("data_train$",paste(relevant.x, collapse= "+data_train$"))))
lambda<-boxCox(model4_formula,objective.name="Log-Likelihood",plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
data_train$Overall<-bcPower(data_train$Overall,lambda = lambda.max)
data_test$Overall<-bcPower(data_test$Overall,lambda = lambda.max)
not_training_features <- c("Overall") 
x_train <- as.matrix(data_train[, ! names(data_train) %in% not_training_features, drop = F])
y_train <- as.matrix(data_train[,"Overall", drop = F])
x_test <- as.matrix(data_test[, ! names(data_test) %in% not_training_features, drop = F])
y_test <- as.matrix(data_test[,"Overall", drop = F])

print("Applied BoxCox. Fitting New Model")
print(data_train$Overall[c(1,3)])
print(data_test$Overall[c(1,3)])

model4_formula <- as.formula(paste("Overall~",paste(relevant.x, collapse= "+")))
model4 <-model4_formula
fit4<-lm(model4,data_train)
summary(fit4)
print("Number of Coeff learnt =")
print(length(fit4$coefficients))
plot(fit4)





# See that the values of R^2 and Adjusted R^2 have increased. 

# Handling Outliers : 


p=dim(data_train)[2]
n=dim(data_train)[1]
print("Original Dim of Training Set = ")
print(dim(data_train))
data_train_outrem <- data_train


# 1. Using Leverage

print("Detecting Outliers using Leverage")
lev<- lm.influence(fit1)$hat
lev_rem <- lev>3*(p+1)/n
print("Number of Outliers Found = ")
print(sum(lev_rem == TRUE))


# 2. Cook's Distance

print("Detecting Outliers using Cook's Distance")
cd<-cooks.distance(fit1)
cd_rem <- cd>1
print("Number of Outliers Found = ")
print(sum(cd_rem == TRUE))


# 3. DFBETAS

print("Detecting Outliers using DFBETAS")
dfb<-dfbeta(fit1)
dim(dfb)
n_dfb = dim(dfb)[1]
p_dfb = dim(dfb)[2]
#Intializing dfb_rem
dfb_rem <- cd_rem
for (i in c(1:n_dfb))
{
  dfb_rem[i]=FALSE
  for (j in c(1:p_dfb))
    if (dfb[i,j]>(2/sqrt(n)))
      dfb_rem[i]<-TRUE
}
print("Number of Outliers Found = ")
print(sum(dfb_rem == TRUE))




# 4. DFFITS

print("Detecting Outliers using DFFITS")
dfft<-dffits(fit1)
dfft_rem <- dfft>(2*sqrt((p+1)/n))
print("Number of Outliers Found = ")
print(sum(dfft_rem == TRUE))





# 5. COVRATIO

print("Detecting Outliers using COVRATIO")
covr<-covratio(fit1)
covr_rem <- ( covr>(1 + (3*((p+1)/n))) | covr<(1 - (3*((p+1)/n)) ) ) 
print("Number of Outliers Found = ")
print(sum(covr_rem == TRUE))





# We will remove all the outliers reported by Leverage, Cook's Distance, DFBETAS, DFFITS, and COVRATIO.
# Then we fit linear model again.


data_train_outrem <- data_train
out_rem <- lev_rem | cd_rem | dfb_rem | dfft_rem | covr_rem
data_train_outrem <- data_train_outrem[out_rem==FALSE,]
dim(data_train_outrem)

lin_model5<-sig.formula
fit5<-lm(lin_model5,data_train_outrem)
summary(fit5)





# See that Value of R^2 Adjusted has increased after handling Outliers.

# Now we predict y for Test Set.


#Checking Dimensions
dim(data_test)
dim(data_train)
dim(x_train)
dim(x_test)

final_prediction <-predict(fit5, newdata=as.data.frame(x_test))
#length(final_prediction)
print("See first 10 Prediction ")
print(final_prediction[c(1:10)])
print("See first 10 Actual ")
print(y_test[c(1:10)])



# Plotting Predicted and Actual Curve for Test Set (Only first 100 Values)

ind = c(1:100)

plot(ind,y_test[c(1:100)],type='l',col="red",xlab="Datapoint Index", ylab="Rating", main="Predicted Rating(Blue) vs Actual Rating(Red)")
lines(ind,final_prediction[c(1:100)],col="blue")




# Finding RMSE
rmse_val <- sqrt(norm(y_test-final_prediction)/n)
print("RMSE=")
print(rmse_val)



# Model Selection Using Stepwise Regression Techniques


# 1. Backward Elimination Method

lin_model6<-sig.formula
data_new<-as.data.frame(data_train)
step_model<-lm(lin_model6,data_new)
ols_step_backward_p(step_model,prem = 0.05)


# 2. Stepwise Elimination Method

lin_model6<-sig.formula
data_new<-as.data.frame(data_train)
step_model<-lm(lin_model6,data_new)
ols_step_both_p(step_model, details = TRUE)


# 3. Step Back AIC Method

lin_model6<-sig.formula
data_new<-as.data.frame(data_train)
step_model<-lm(lin_model6,data_new)
ols_step_backward_aic(step_model,details = TRUE)


# 4. Step Forward AIC Method

lin_model6<-sig.formula
data_new<-as.data.frame(data_train)
step_model<-lm(lin_model6,data_new)
ols_step_forward_aic(step_model,details = TRUE)


