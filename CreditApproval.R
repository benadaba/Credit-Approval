setwd("C:/Users/adaba/Documents/Coursera/Credit Card Approval")
library(data.table)

#credit.lisp 
credit.lisp <- data.table(read.table("credit.lisp.txt"))
head(credit.lisp)
str(credit.lisp)

#
crx.data <- data.table(read.table("crx.data.txt", header = FALSE, sep = ",", na.strings = "?"))
head(crx.data)
str(crx.data)
names(crx.data)

#lets find the classes of the features /variables of the dataset
sapply(crx.data, class)

#distribution of the class variable
# +  = 1 and -  = 0
hist(as.numeric(crx.data$V16)-1)
as.numeric(crx.data$V16)-1



#lets changed the target variable to 0s and 1s
crx.data$V16 <- as.numeric(crx.data$V16)-1
crx.data$V16



# create histograms for each attribute
numeric_data <- as.data.frame(crx.data[,c(2,3,8,11,14,15)])
summary(numeric_data)
par(mfrow=c(2,3))
for(i in 1:6) {
    hist(numeric_data[,i], main=names(numeric_data)[i])
}


#look closely at V15
par(mfrow=c(1,1))
hist((numeric_data[,"V15"]))
#hist(log10(numeric_data[,"V15"]))
summary(numeric_data$V15)


#non numberical values 
non_numeric <- numeric_data <- as.data.frame(crx.data[,-c(2,3,8,11,14,15)])
non_numeric
par(mfrow=c(2,5))
for(i in 1:10) {
    plot(non_numeric[,i], main=names(non_numeric)[i])
}
dim(non_numeric)
#plot(table(non_numeric[,1]))

#relationship between the numerical features of the application
plot(crx.data[,c(2,3,8,11,14,15)], col=crx.data$V16)

#correlations between the numerical features
correlations <- cor(crx.data[,c(2,3,8,11,14,15)])
print(correlations)


#lets get a pairwise visualization of all the dataset
pairs(V16~., data=non_numeric, col=non_numeric$V16)


#summary
summary(crx.data)

#1). V8 seems to have an outlier as 75% of the population have less than or 2 value but the max figure
# is 28

#what percentage of the dataset is missing some VALUES. if more than 10% we should go and find
#more than appropriate data
#will this percentage of missen data have an impact on our ?
#how do we handle missen data
missen <- sum(!complete.cases(crx.data))/dim(crx.data)[1] *100
missen  ## about 5.36% missen data 


###HANDLING MISSEN VALUES

#input the missen values in the variable 1 (V1) with the most occuring value - MODE
# Create function to get the mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

v1.mode <- getmode(crx.data$V1)

#input the missen values with the mode
crx.data[is.na(crx.data$V1), 1]  <- v1.mode

#preview the inputation
table(crx.data$V1)


#input the missen values in the variable V2 with the average
crx.data[is.na(crx.data$V2),  2]  <- mean(crx.data$V2, na.rm = TRUE)
#preview the inputation
summary(crx.data$V2)

summary(crx.data)


#input the missen values in the V4 with the mode
crx.data[is.na(crx.data$V4), 4]  <- getmode(crx.data$V4)
#preview the inputation
table(crx.data$V4)


#input the missen values in the V5 with the mode
crx.data[is.na(crx.data$V5), 5]  <- getmode(crx.data$V5)
#preview the inputation
table(crx.data$V5)

#V6: input the missen values  with the mode
crx.data[is.na(crx.data$V6), 6]  <- getmode(crx.data$V6)
#preview the inputation
table(crx.data$V6)

#any missen values
table(is.na(crx.data$V6))

#V7: input the missen values  with the mode
crx.data[is.na(crx.data$V7), 7]  <- getmode(crx.data$V7)
#preview the inputation
table(crx.data$V7)

#any missen values
table(is.na(crx.data$V7))
#input the 
summary(crx.data)


#V14: input the missen values  with the mode
crx.data[is.na(crx.data$V14), 14]  <- as.integer(mean(crx.data$V14, na.rm = TRUE))
#preview the inputation
summary(crx.data$v14)



#lets handle the categorical variables. 
sapply(non_numeric, class)
#cntr <- contrasts(non_numeric[,-c(V16)])

#install.packages("mlbench")
#install.packages("caret")
library(mlbench)
library(caret)
#lets transform the data
#preprocessParams <- preProcess(crx.data[,1:15], method=c("scale"))
preprocessParams <- preProcess(crx.data, method=c("center", "scale", "pca"))
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, crx.data[,-"V16"])
# summarize the transformed dataset
summary(transformed)


#split data into test and train set
# define an 70%/30% train/test split of the dataset
trainIndex <- createDataPartition(crx.data$V16, p=0.70, list=FALSE)
dataTrain <- crx.data[ trainIndex,]
dataTest <- crx.data[-trainIndex,]


model <- glm(V16 ~.,family=binomial(link='logit'),data=dataTrain)

summary(model)


#Now we can run the anova() function on the model to analyze the table of deviance
anova(model, test="Chisq")


# make predictions
probabilities <- predict(model, newdata = dataTest[,-16], type='response')
predictions <- ifelse(probabilities > 0.5,'1','0')
# summarize accuracy
table(predictions, dataTest$V16)

misClasificError <- mean(predictions != dataTest$V16)
print(paste('Accuracy',1-misClasificError))

#match predictions to original dataset
copy_dataTest <- data.frame(dataTest)
dim(copy_dataTest)
copy_dataTest_pred <- cbind(copy_dataTest, predictions)
copy_dataTest_pred

#lets check the area under the curve
#install.packages("ROCR")
library(ROCR)
predicted <- predict(model, newdata=dataTest[,-16], type="response")
pr <- prediction(predicted, dataTest$V16)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc