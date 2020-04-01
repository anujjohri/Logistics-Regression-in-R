# Import dataset

turnout <- read.csv(file = "C:/Users/Hp/Desktop/data science/R/Datasets/turnout.csv")  
str(turnout)

#Removing the irrelavent column
turnout$X <- NULL

#checking tha data type
names(turnout)
str(turnout)

# Variable conversion of Target variable
turnout$vote <- as.factor(turnout$vote)
table(turnout$vote)

# converting categorical data to numeric 

# Creating a new column to represent race
turnout$race_n <- as.factor(ifelse(turnout$race == 'white' , 0 , 1))
table(turnout$race_n)

# Removing duplicate column
turnout$race <- NULL

str(turnout)

# Check missing value

sapply(data, function(x) sum(is.na(turnout)))

# Outlier check

boxplot(turnout$age)       #no
boxplot(turnout$educate)  #yes lower
boxplot(turnout$income)  # yes upper



# Treatment of outlier for educate

summary(turnout$educate)
lower <- 10.0 - 1.5* IQR(turnout$educate)  
lower
turnout$educate [turnout$educate < lower] <- lower
boxplot(turnout$educate)
summary(turnout$educate)

# Treatment of outlier for income

summary(turnout$income)
upper <- 5.233 + 1.5* IQR(turnout$income)  
upper
turnout$income [turnout$income > upper] <- upper
boxplot(turnout$income)
summary(turnout$income)

#data partition

set.seed(100)
library(caret)
Train <- createDataPartition(turnout$vote , p=0.7 , list = FALSE)
training <- turnout[ Train , ]
testing <- turnout[ -Train , ]

# Model building

logit <- glm(vote~. , family = 'binomial' , data = training)
summary(logit)   # AIC = 1434.4
#need to remove race variable as it does not have any impact

# Creating another model 
logit2 <- step(glm(vote~. , family = 'binomial' , data = training), direction = 'backward')
summary(logit2)     #AIC = 1430

# Checking correlation
library(car)
vif(logit2)

# ODDS RATIO
# Checking concordance, disconcordance and tie pair
# Running a predefined function 


Acc(logit2)  #Percent Concordance - 72%


exp(coef(logit2)) 
cbind( odds_ratio = exp(coef(logit2)) ,exp(confint(logit2)) )


logit2$coefficients 


# PREDICTION on Testing
testing$probs <- predict(logit2 , testing, type = 'response')
testing$Predict <- as.factor(ifelse(testing$probs > 0.70 , 1 , 0))

# Checing Accuracy
table(testing$Predict , testing$vote)
confusionMatrix( testing$vote , testing$Predict)


library(ROCR)

#Predictions on training set 
predictTrain = predict(logit2 , testing , type = 'response')

# ROC Curve

#prediction function
ROCRpred = prediction(predictTrain , testing$vote)

# performance function
ROCRpref = performance(ROCRpred , "tpr" , "fpr")

#plot ROC curve 
plot(ROCRpref)
library(ROCR)

pred = prediction(testing$probs , testing$vote)
as.numeric(performance(pred, "auc") @y.values)  


# K- fold validation

library(caret)
crossValSettings <- trainControl(method = "repeatedcv" ,
                                 number = 10 ,
                                 savePredictions = TRUE)

crossVal <- train(as.factor(vote) ~ age + educate + 
                    income   , 
                  data = turnout , 
                  family = "binomial" ,
                  method= "glm" ,
                  trControl = crossValSettings)
crossVal
summary(crossVal)



