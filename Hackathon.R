install.packages("DMwR")
install.packages("caret")
install.packages("caTools")
install.packages("purrr")
install.packages("tidyr")
install.packages("ggplot2")
library(purrr)
library(tidyr)
library(ggplot2)
library(caTools)
library(caret)
library(DMwR)

#Read dataset into R
Data= read.csv("Data1.csv", header = TRUE)

#Change Column Names
colnames(Data)[1:76]<- c("Customer",
                         "Number.of.houses",
                         "Avg.size.household",
                         "Avg.age",
                         "Customer.main.type",
                         "Roman.catholic",
                         "Protestant",
                         "Other.religion",
                         "No.religion",
                         "Married",
                         "Living.together",
                         "Other.relation",
                         "Singles",
                         "Household.without.children",
                         "Household.with.children",
                         "High.level.education",
                         "Medium.level.education",
                         "Lower.level.education",
                         "High.status",
                         "Entrepreneur",
                         "Farmer",
                         "Middle.management",
                         "Skilled.labourers",
                         "Unskilled.labourers",
                         "Social.class.A",
                         "Social.class.B1",
                         "Social.class.B2",
                         "Social.class.C",
                         "Social.class.D",
                         "Rented.house",
                         "Home.owners",
                         "X1.car",
                         "X2.cars",
                         "No.car",
                         "National.Health.Service",
                         "Private.health.insurance",
                         "Income...30.000",
                         "Income.30.45.000",
                         "Income.45.75.000",
                         "Income.75.122.000",
                         "Income..123.000",
                         "Average.income",
                         "Purchasing.power.class",
                         "Contribution.private.third.party.insurance.see.L4",
                         "Contribution.third.party.insurance..firms.",
                         "Contribution.third.party.insurance..agriculture.",
                         "Contribution.car.policies",
                         "Contribution.delivery.van.policies",
                         "Contribution.motorcycle.scooter.policies",
                         "Contribution.lorry.policies",
                         "Contribution.trailer.policies",
                         "Contribution.tractor.policies",
                         "Contribution.agricultural.machines.policies",
                         "Contribution.moped.policies",
                         "Contribution.life.insurances",
                         "Contribution.private.accident.insurance.policies",
                         "Contribution.family.accidents.insurance.policies",
                         "Contribution.disability.insurance.policies",
                         "Contribution.fire.policies",
                         "Contribution.surfboard.policies",
                         "Contribution.boat.policies",
                         "Contribution.bicycle.policies",
                         "Contribution.property.insurance.policies",
                         "Contribution.social.security.insurance.policies",
                         "Number.of.private.third.party.insurance.1...12",
                         "Number.of.third.party.insurance..firms.",
                         "Number.of.third.party.insurance..agriculture.",
                         "Number.of.car.policies",
                         "Number.of.delivery.van.policies",
                         "Number.of.motorcycle.scooter.policies",
                         "Number.of.lorry.policies",
                         "Number.of.trailer.policies",
                         "Number.of.tractor.policies",
                         "Number.of.agricultural.machines.policies",
                         "Number.of.moped.policies",
                         "OUTCOME"
)

#convert Categorical Columns to factor from Numeric
Data$OUTCOME <- as.factor(Data$OUTCOME)

#Study the Dataset
str(Data)

levels(Data$OUTCOME)[levels(Data$OUTCOME)=="1"] <- "Yes"
levels(Data$OUTCOME)[levels(Data$OUTCOME)=="0"] <- "No"

#names <- c(76)
#Data[,names] <- lapply(Data[,names] , factor)

# Visualizing all NUmeric Variables -



Data[,1:75] %>%
keep(is.numeric) %>% 
gather() %>% 
ggplot(aes(value)) +
facet_wrap(~ key, scales = "free") +
geom_histogram()

#Removing Columns that are not not imortant enough and causing issues in computation

Data$Contribution.lorry.policies <- NULL
Data$Number.of.lorry.policies <- NULL
Data$Contribution.boat.policies <- NULL
Data$Contribution.agricultural.machines.policies <- NULL
Data$Contribution.surfboard.policies<- NULL
Data$Number.of.agricultural.machines.policies <- NULL
Data$Contribution.lorry.policies <- NULL
Data$Number.of.lorry.policies <- NULL


#ggplot(data = Data)+geom_bar(aes(x = Data$Customer, fill=Data$OUTCOME))



#Spliting the dataset into train and test
set.seed(123)
split = sample.split(Data$OUTCOME, SplitRatio = 0.80)
train = subset(Data, split == TRUE)
test = subset(Data, split == FALSE)

#Scaling the dataset
train[,-c(70)] = scale(train[,-c(70)])
test[,-c(70)] = scale(test[,-c(70)])

#REmoving the NULL columns 
train$Contribution.disability.insurance.policies <- NULL
test$Contribution.disability.insurance.policies <- NULL

#check for the number of yes no values
table(train$OUTCOME)
table(test$OUTCOME)

#apply PCA
pca = preProcess(x = train[-69], method = 'pca', pcaComp = 8)
train = predict(pca, train)
train = train[c(2,3,4,5,6,7,8,9,1)]
test = predict(pca, test)
test = test[c(2,3,4,5,6,7,8,9,1)]

#Using SMOTE to Balance the training data
train_smote <- SMOTE(train$OUTCOME~., data = train, perc.over = 250, perc.under = 350)

#Check the ratio of the Outcome in training set
table(train_smote$OUTCOME)

#Setting up parameters for model training
control = trainControl(
  method = 'cv',
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

#Training the model, Caret packge used and Naive Baise algorithm
nb_smote <- train(OUTCOME~ .,
                   data = train_smote,
                   method = "nb",
                   metric = "Sens",
                   trControl = control)

#predict on the test set
pred <- predict(nb_smote, test)

#Creating confusion matrix and rint the important statistics
confusionMatrix(predict(nb_smote, test), test$OUTCOME)
