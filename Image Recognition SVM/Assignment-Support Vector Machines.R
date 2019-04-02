# Support Vector Machines Assignment
# PGDDS June 2018 Cohort
# Shailesh Kumar Jha

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# 1. Business Understanding


# A classic problem in the field of pattern recognition is that
# of handwritten digit recognition. Suppose that you have an image 
# of a digit submitted by a user via a scanner, a tablet, or other 
# digital devices. The goal is to develop a model that can correctly 
# identify the digit (between 0-9) written in an image. 




# -----------------------------------------------------------------------
# 2. Data Sourcing


train<-read.csv("mnist_train.csv",header = FALSE)
test<-read.csv("mnist_test.csv", header = FALSE)


# Loading the required padckages

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)




# ------------------------------------------------------------------------
# 3. Data Preparation


# 3.1 Fixing Rows- No unnecessary header and footer rows,
# no summary rows, no indicator rows, no blank rows, Some rows maybe 
# repetetive which are remooved

# 3.2 Fixing Columns- 1st column name missing, removing columns having
# same value for all records, no need for renaming other columns,
# no misaligned columns


colnames(train)[1]<-"digits"
colnames(test)[1]<-"digits"

redundant<-sapply(train,function(x) length(unique(x))>1)
train<-train[redundant]
test<-test[redundant]
    
# 3.3 Checking missing values if any

sum(is.na(train))
sum(is.na(test))

# 4.4 Will standardise quantitative variables in EDA, after manually
# sifting no invalid values are present and if any will do in EDA, 
# will filter the data if whenever required.




# ------------------------------------------------------------------------
# 4. Data Understanding 


# 4.1 Outlier Treatment- No outliers

sapply(train[,-1],min)
min(sapply(train[,-1],min))

sapply(train[,-1],max)
max(sapply(train[,-1], max))


sapply(test[,-1],min)
min(sapply(train[,-1],min))

sapply(test[,-1],max)
max(sapply(train[,-1], max))


# 4.2 Digit distribution- Uniformly distributed


train$digits<-as.factor(train$digits)
test$digits<-as.factor(test$digits)

digit_obj_train<-ggplot(train,aes(x=train$digits))
digit_plot_train<-digit_obj_train+geom_bar(fill="lavender",color="black",stat="count")+xlab(" Digits ")
digit_plot_train

digit_obj_test<-ggplot(test,aes(x=as.factor(test$digits)))
digit_plot_test<-digit_obj_test+geom_bar(fill="peachpuff3",color="black",stat="count")+xlab(" Digits ")
digit_plot_test




# ------------------------------------------------------------------------
# 5. Modelling


# 5.1 Sampling
# Taking 10000 sample for training and 2000 for cross validation

set.seed(10)
train_final_sample<-sample(1:nrow(train),10000)

set.seed(15)
cross_val_sample<-sample(1:nrow(train),2000)

train_final<-train[train_final_sample,]
cross_val<-train[cross_val_sample,]


# 5.2 Linear SVM with C=1

model_1<-ksvm(digits~.,train_final,scale=FALSE,C=1)

prediction_1<-predict(model_1,newdata=test)

confusionMatrix(prediction_1,test$digits)


# 5.3 Linear SVM with Costof missclasification C = 10

model_2<-ksvm(digits~.,train_final,scale=FALSE,C=10)

prediction_2<-predict(model_2,newdata=test)

confusionMatrix(prediction_2,test$digits)


# 5.4 Linear Model Hyperparameter tuning and cross validation 

trainc<-trainControl(method = "cv",number = 5)
metric<-"Accuracy"
set.seed(7)
lgrid<-expand.grid(c=seq(1,10,by=1))

lin.svm<-train(digits~., data=train_final,method="svmLinear",metric=metric,
               tunegrid=lgrid,trControl=trainc)

print(lin.svm)


# 5.5 Non linear SVM models - RBF Kernel

model_nonlin_rbf_1<-ksvm(digit~., data= train_final,scale=FALSE,kernel="rbfdot")

prediction_rbf_1<-predict(model_nonlin_rbf_1,newdata=test)

confusionMatrix(prediction_rbf_1,test$digits)





# ------------------------------------------------------------------------
# END ---------------------------------------------------------------------


