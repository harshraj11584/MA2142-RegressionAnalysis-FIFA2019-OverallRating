library(readxl)
data_original <- read.csv("/home/harsh/Desktop/Regression_Analysis/Regression_Project/data.csv")
data_original

cols.dont.want <- c("X","ID","Name", "Photo", "Nationality", "Flag", "Potential", "Club", "Club.Logo", "Preferred.Foot", "Work.Rate", "Body.Type", "Real.Face", "Position", "Jersey.Number", "Joined", "Loaned.From", "Contract.Valid.Until" ) 
data <- data_original[, ! names(data_original) %in% cols.dont.want, drop = F]
data


data$Value <- gsub("€", "", data$Value)
data$Value <- as.numeric(gsub("M", "", data$Value))
data$Release.Clause <- gsub("€", "", data$Release.Clause)
data$Release.Clause <- as.numeric(gsub("M", "", data$Release.Clause))
data$Wage <- gsub("€", "", data$Wage)
data$Wage <- as.numeric(gsub("K", "", data$Wage))
data$Height <- as.numeric(gsub("\'", ".", data$Height))
data$Weight <- as.numeric(gsub("lbs", "", data$Weight))

for (i in c(11:38))
  data[,i]<- as.numeric(gsub("+", ".", data[,i],fixed=TRUE))
data

for(i in 1:ncol(data))
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
data


data_shuffled <- data[sample(nrow(data)),]
#data_shuffled <- data_shuffled[,-1] #removing the indexing column that shuffling induced
r1 <- as.integer(nrow(data_shuffled)*0.7) #row number at which splitting takes place
data_train <- data_shuffled[1:r1,]
data_test <- data_shuffled[(r1+1):nrow(data_shuffled),]

data_shuffled
data_train
data_test



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

#fitting the training dataset

model<-Overall~Reputation+Weak_Foot+Skill_Moves+Height+Weight+LS+ST+RS+LW+LF+CF+RF+RW+LAM+CAM+RAM+LM+LCM+CM+RCM+RM+LWB+LDM+CDM+RDM+RWB+LB+LCB+CB+RCB+RB+Crossing+Finishing+HeadingAccuracy+ShortPassing+Volleys+Dribbling+Curve+FKAccuracy+LongPassing+BallControl+Acceleration+SprintSpeed+Agility+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+Aggression+Interceptions+Positioning+Vision+Penalties+Composure+Marking+StandingTackle+SlidingTackle+GKDiving+GKHandling+GKKicking+GKPositioning+GKReflexes+Value
fit1<-lm(model,data_train)
fit1
summary(fit1)
#Residuals:
#  Min      1Q     Median   3Q     Max 
#-12.504  -1.595   0.010   1.615  12.306 
# Residual standard error: 2.478
# Multiple R-squared:  0.8713,	Adjusted R-squared:  0.8707 
plot(fit1)

#Applying Box-Cox transformation and checking the acccuracy using objective of log-likelihood

lambda<-boxcox(data_train$Overall~data_train$Reputation+data_train$Weak_Foot+data_train$Skill_Moves+data_train$Height+data_train$Weight+data_train$LS+data_train$ST+data_train$RS+data_train$LW+data_train$LF+data_train$CF+data_train$RF+data_train$RW+data_train$LAM+data_train$CAM+data_train$RAM+data_train$LM+data_train$LCM+data_train$CM+data_train$RCM+data_train$RM+data_train$LWB+data_train$LDM+data_train$CDM+data_train$RDM+data_train$RWB+data_train$LB+data_train$LCB+data_train$CB+data_train$RCB+data_train$RB+data_train$Crossing+data_train$Finishing+data_train$HeadingAccuracy+data_train$ShortPassing+data_train$Volleys+data_train$Dribbling+data_train$Curve+data_train$FKAccuracy+data_train$LongPassing+data_train$BallControl+data_train$Acceleration+data_train$SprintSpeed+data_train$Agility+data_train$Reactions+data_train$Balance+data_train$ShotPower+data_train$Jumping+data_train$Stamina+data_train$Strength+data_train$LongShots+data_train$Aggression+data_train$Interceptions+data_train$Positioning+data_train$Vision+data_train$Penalties+data_train$Composure+data_train$Marking+data_train$StandingTackle+data_train$SlidingTackle+data_train$GKDiving+data_train$GKHandling+data_train$GKKicking+data_train$GKPositioning+data_train$GKReflexes+data_train$Value,objective.name="Log-Likelihood",plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
Overall.tr<-bcPower(data_train$Overall,lambda = lambda.max)
model2 <-Overall.tr~Reputation+Weak_Foot+Skill_Moves+Height+Weight+LS+ST+RS+LW+LF+CF+RF+RW+LAM+CAM+RAM+LM+LCM+CM+RCM+RM+LWB+LDM+CDM+RDM+RWB+LB+LCB+CB+RCB+RB+Crossing+Finishing+HeadingAccuracy+ShortPassing+Volleys+Dribbling+Curve+FKAccuracy+LongPassing+BallControl+Acceleration+SprintSpeed+Agility+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+Aggression+Interceptions+Positioning+Vision+Penalties+Composure+Marking+StandingTackle+SlidingTackle+GKDiving+GKHandling+GKKicking+GKPositioning+GKReflexes+Value
fit2<-lm(model2,data_train)
summary(fit2)

# Residuals:
#    Min       1Q      Median    3Q      Max
# -246.692  -38.340    0.008   38.794  263.503 
# Residual standard error: 58.66
# Multiple R-squared:  0.877,	Adjusted R-squared:  0.8764 

rest<-residuals(fit2)
pyt<-predict(fit2)
plot(pyt,rest)
plot(fit2)

#?shapiro.test
#is.numeric(data_train$Overall)
#shapiro.test(data-train$Overall) **** shapiro normality test could not be applied due to szie of rows are out of range

# Giving weights to the regressors on fit1

wts <- 1/fitted(lm(abs(residuals(fit1)) ~ fitted(fit1)))^2
fit3 <-lm(model, weights = wts,data_train)
summary(fit3)
# Weighted Residuals:
#   Min      1Q    Median     3Q     Max
# -5.5061 -0.8125  0.0178  0.8388  6.0383
# Residual standard error: 1.265
# Multiple R-squared:  0.8806,	Adjusted R-squared:   0.88 
plot(fit3)

# Giving weights to the regressors on fit2
wts <- 1/fitted(lm(abs(residuals(fit2)) ~ fitted(fit2)))^2
fit4 <-lm(model2, weights = wts,data_train)
summary(fit4)
# Weighted Residuals:
#  Min        1Q   Median   3Q       Max 
# -5.3541 -0.8242  0.0000  0.8347  5.7237 
# Residual standard error: 1.263
# Multiple R-squared:  0.8796,	Adjusted R-squared:  0.879 
plot(fit4)

