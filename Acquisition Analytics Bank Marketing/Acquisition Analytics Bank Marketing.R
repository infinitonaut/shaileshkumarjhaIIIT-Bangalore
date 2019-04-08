## Acquisition analytics assignment
## PGDDS June 2018 cohort
## Shailesh Kumar Jha


# ------------------------------------------------------------------------


##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations


#-------------------------------------------------------
## 1. Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

# Checking missing values

sum(is.na(bank_data))

# Checking duplicate values and removing them

sum(duplicated(bank_data))
nrow(unique(bank_data))
bank_data<-unique(bank_data)

# Creating unique prospectID for each of the prospects

bank_data$prospectid<-seq.int(nrow(bank_data))

#-------------------------------------------------------

# Checking response rate of prospect customer

response_rate<-nrow(subset(bank_data,response=='yes'))/nrow(bank_data)
response_rate
# 11.266% is the response rate 


#-------------------------------------------------------


## Univariate Analysis 


# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Outlier treatement in Age 
quantile(bank_data$age,seq(0,1,0.01))
boxplot(bank_data$age)

# Box plot shows quiet a no of outliers. Capping it to 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age" for the purpose of analysis.
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "prospects_responded","total_prospects")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))


# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, prospects_responded,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
# Bank_data_age20 <- subset(bank_data,age <20)

# View(Bank_data_age20)
# summary(Bank_data_age20)

# Observations:
# Age groups 30-40 and 

## Also removeing age bin which is not required anymore.
bank_data$binning.age <- NULL


#  Writing A REUSABLE function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + 
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

# job

levels(bank_data$job)
plot_response(bank_data$job, "job")

# Marital status
summary(bank_data$marital)
levels(bank_data$marital)[4] <- "married"
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Replotting education
plot_response(bank_data$education,"Education_levels")

# Default 
table(bank_data$default)
plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

# housing variables 
summary(bank_data$housing)
plot_response(bank_data$housing, "Housing")

# "loan"
summary(bank_data$loan)
plot_response(bank_data$loan, "Loan Status")


# ------------------------------------------------------------------------


# EDA on Campaign Related Variables

# Contact 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

# "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

# "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

# "duration" variable: No Analysis done as it will not be used in the model. 

# "campaign" variable
# (number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summary of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)
quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

# "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)


# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")


# ------------------------------------------------------------------------


# EDA of Socio economic variables

# emp.var.rate :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)



# ------------------------------------------------------------------------


#-------------------------------------------------------
## 2. Logistic Regression
#-------------------------------------------------------
# Perform variable selection using the usual methods
# Sort the data points in decreasing order of probability of response
# Find the optimal probability cut-off and report the relevant evaluation metrics


# Loading Required Packages

library(caret)
library(caTools)
library(dummies)

# Creating dummy variables
# Converting target variable to integer so that dummy variables are not created automatically.
View(bank_data)

bank_data$response <- as.integer(bank_data$response)
summary(bank_data$response)

bank_data_bkp <- bank_data  # backup dataset, to use if needed.
bank_data <- dummy.data.frame(bank_data)

# Converting the response variableback to factor.
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(100)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train <- bank_data[split_indices, ]
test <- bank_data[!split_indices, ]

# Loading required Libraries 

library(MASS)
library(car)


# Model 1
# Building the first model without duration as per the business understanding
# and also without prospectid as it seems meaningless

model_1<-glm(response~. -duration -prospectid,family = "binomial",data=train)
summary(model_1)


# Model 2
# Applying stepAIC to model_1

model_2<-stepAIC(model_1,direction = "both")
summary(model_2)
# stepAIC has removed most of the variables 


# Model 3

model_3<-glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
               educationPrimary_Education + educationSecondary_Education + 
               housingno + contactcellular + monthaug + monthdec + monthjun + 
               monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
               campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
               `previousNever contacted` + poutcomefailure + emp.var.rate + 
               cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
               `jobblue-collar`, family = "binomial", data = train)

vif(model_3)
summary(model_3)
# Removing euribor3m and creating subsequent model


# Model 4

model_4<-glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
               educationPrimary_Education + educationSecondary_Education + 
               housingno + contactcellular + monthaug + monthdec + monthjun + 
               monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
               campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
               `previousNever contacted` + poutcomefailure + emp.var.rate + 
               cons.price.idx + cons.conf.idx + nr.employed + 
               `jobblue-collar`, family = "binomial", data = train)

vif(model_4)
summary(model_4)
# Removing 'previousNever contacted' and creating subsequent model


# Model 5

model_5<-glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
               educationPrimary_Education + educationSecondary_Education + 
               housingno + contactcellular + monthaug + monthdec + monthjun + 
               monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
               campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
               cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
             family = "binomial", data = train)

vif(model_5)
summary(model_5)
# Removing housingno and creating subsequent model


# Model 6

model_6<-glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
               educationPrimary_Education + educationSecondary_Education + contactcellular + monthaug + monthdec + monthjun + 
               monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
               campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
               poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
               nr.employed + `jobblue-collar`, family = "binomial", data = train)

vif(model_6)
summary(model_6)
# Now based only on p-values removing jobadmin. and creating subsequent model


# Model 7

model_7<-glm(formula = response ~ age + jobretired + jobstudent + 
               educationPrimary_Education + educationSecondary_Education + 
               contactcellular + monthaug + monthdec + monthjun + monthmar + 
               monthmay + monthnov + day_of_weekfri + day_of_weekmon + campaign + 
               pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
               poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
               nr.employed + `jobblue-collar`, family = "binomial", data = train)

vif(model_7)
summary(model_7)
# Now based on p-value alone removing monthdec and creating subsequent model


# Model 8

model_8<-glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
               educationSecondary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
               day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
               pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
               cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
             family = "binomial", data = train)

vif(model_8)
summary(model_8)
# Now based on p-value alone removing educationPrimary_Education and creating subsequent model


# Model 9

model_9<-glm(formula = response ~ age + jobretired + jobstudent + 
               educationSecondary_Education + contactcellular + monthaug + 
               monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
               day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
               pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
               cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
             family = "binomial", data = train)

vif(model_9)
summary(model_9)
# Now based on p-value removing educationSecondary_Education and creating subsequent model


# Model 10

model_10<-glm(formula = response ~ age + jobretired + jobstudent + 
                contactcellular + monthaug + monthjun + monthmar + monthmay + 
                monthnov + day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
              family = "binomial", data = train)

vif(model_10)
summary(model_10)
# Now based on p-value removing age and creating subsequent model


# Model 11

model_11<-glm(formula = response ~ jobretired + jobstudent + contactcellular + 
                monthaug + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
              family = "binomial", data = train)

vif(model_11)
summary(model_11)
# Now based on p-value alone removing day_of_weekfri and creating subsequent model


# Model 12

model_12<-glm(formula = response ~ jobretired + jobstudent + contactcellular + 
                monthaug + monthjun + monthmar + monthmay + monthnov + 
                day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                cons.price.idx + cons.conf.idx + nr.employed + `jobblue-collar`, 
              family = "binomial", data = train)

vif(model_12)
summary(model_12)
# Based on p-value alone removing jobretired


# Model 13

model_13<-glm(formula = response ~ jobstudent + contactcellular + 
                monthaug + monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                nr.employed + `jobblue-collar`, family = "binomial", data = train)
vif(model_13)
summary(model_13) 
# Based on p-value alone removing monthaug


# Model 14

model_14<-glm(formula = response ~ jobstudent + contactcellular + 
                monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                nr.employed + `jobblue-collar`, family = "binomial", data = train)

vif(model_14)
summary(model_14)
# Based on p-value alone removing nr.employed


# Model 15

model_15<-glm(formula = response ~ jobstudent + contactcellular + monthjun + 
                monthmar + monthmay + monthnov + day_of_weekmon + campaign + 
                pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + `jobblue-collar`, family = "binomial", data = train)

vif(model_15)
summary(model_15)
# Now model_15 is the final model

# Final model summary
# Call:
#  glm(formula = response ~ jobstudent + contactcellular + monthjun + 
#        monthmar + monthmay + monthnov + day_of_weekmon + campaign + 
#        pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
#        poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
#        `jobblue-collar`, family = "binomial", data = train)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0651  -0.3881  -0.3254  -0.2664   2.8860  

#Coefficients:
#  Estimate Std. Error z value
#(Intercept)                    -1.062e+02  5.251e+00 -20.231
#jobstudent                      3.826e-01  1.035e-01   3.695
#contactcellular                 6.430e-01  6.888e-02   9.335
#monthjun                       -3.013e-01  7.546e-02  -3.992
#monthmar                        1.161e+00  1.168e-01   9.941
#monthmay                       -6.230e-01  5.966e-02 -10.442
#monthnov                       -4.942e-01  7.697e-02  -6.420
#day_of_weekmon                 -2.784e-01  5.412e-02  -5.143
#campaign                       -3.847e-02  1.166e-02  -3.301
#pdaysContacted_in_first_10days  1.266e+00  8.460e-02  14.967
#pdaysContacted_after_10days     1.239e+00  1.668e-01   7.427
#poutcomefailure                -5.853e-01  6.578e-02  -8.898
#emp.var.rate                   -7.547e-01  2.071e-02 -36.453
#cons.price.idx                  1.125e+00  5.647e-02  19.929
#cons.conf.idx                   3.559e-02  4.223e-03   8.427
#`jobblue-collar`               -2.457e-01  5.875e-02  -4.183
#Pr(>|z|)    
#(Intercept)                     < 2e-16 ***
#  jobstudent                     0.000220 ***
#  contactcellular                 < 2e-16 ***
#  monthjun                       6.55e-05 ***
#  monthmar                        < 2e-16 ***
#  monthmay                        < 2e-16 ***
#  monthnov                       1.36e-10 ***
#  day_of_weekmon                 2.70e-07 ***
#  campaign                       0.000964 ***
#  pdaysContacted_in_first_10days  < 2e-16 ***
#  pdaysContacted_after_10days    1.11e-13 ***
#  poutcomefailure                 < 2e-16 ***
#  emp.var.rate                    < 2e-16 ***
#  cons.price.idx                  < 2e-16 ***
#  cons.conf.idx                   < 2e-16 ***
#  `jobblue-collar`               2.88e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 20293  on 28822  degrees of freedom
#Residual deviance: 15905  on 28807  degrees of freedom
#AIC: 15937

#Number of Fisher Scoring iterations: 6

# ------------------------------------------------------------------------


# Predicting probabnilities of responding for the test data

test$pred_probability<-predict(model_15,newdata = test[,-61],type = "response")
predictions_logistic<-predict(model_15,newdata = test[,-61],type = "response")


# Let us use the probability cutoff of 50% and check confusion matrix

predicted_response<-factor(ifelse(predictions_logistic>0.5,"yes","no"))
conf<-confusionMatrix(predicted_response,test$response,positive = "yes")
conf
# This results in poor sensitivity


# Finding the optimal probability cutoff and reporting the evaluation metrics

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logistic >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)

summary(test$response)
summary(predicted_response)

levels(test$response)
levels(predicted_response)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# Plotting cutoffs

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

#Choosing a cutoff of 9% for the final model_1

predicted_response <- factor(ifelse(predictions_logistic >= 0.09, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec


# At optimal probability cutoff of 0.09 the evaluation metrics are as follows

# Accuracy      Sensitivity     Specificity 
# 0.7742249       0.6752874       0.7867895 
              

# ------------------------------------------------------------------------


#-------------------------------------------------------
## 3. Creating the new required data frame
#-------------------------------------------------------


# Create a data frame with the variables prospect ID, actual response, 
# predicted response, predicted probability of response, 
# duration of call in seconds, and cost of call
# While creating the data frame, calculating the cost of call for each prospect in a new column


test$predicted_response<-predicted_response

# Creating data frame
prospect_df<-test[,c("prospectid","response","predicted_response","pred_probability","duration")]
colnames(prospect_df)<-c("prospectid","actualresponse","predictedresponse","predictedprobability","duration")
View(prospect_df)

# Calculating cost of call
prospect_df$costofcall<-0.033*(prospect_df$duration)+0.8

# Sorting the probabilities in decreasing order
prospect_df<-prospect_df[order(prospect_df$predictedprobability,decreasing = TRUE),]

# Downloading the data
write.csv(prospect_df,"response_prediction_via_logistic.csv")


# ------------------------------------------------------------------------


#-------------------------------------------------------
## 4. Finding top X% prospects to meet the business objective
#-------------------------------------------------------


# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

summary(prospect_df$actualresponse)

prospect_df$actualresponse <- as.factor(ifelse(prospect_df$actualresponse=="yes",1,0))
prospect_df$predictedresponse<-as.factor(ifelse(prospect_df$predictedresponse=="yes",1,0))

LG = lift(prospect_df$actualresponse, prospect_df$predictedprobability, groups = 10)
LG

# A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
# 1      1  1236       614     614  44.1    4.41
# 2      2  1235       232     846  60.8    3.04
# 3      3  1235       131     977  70.2    2.34
# 4      4  1236        85    1062  76.3    1.91
# 5      5  1235        76    1138  81.8    1.64
# 6      6  1235        60    1198  86.1    1.43
# 7      7  1236        69    1267  91.0    1.30
# 8      8  1235        43    1310  94.1    1.18
# 9      9  1235        43    1353  97.2    1.08
#10     10  1235        39    1392 100      1 

# So from the gain table, as per the business objective to 
#achieve 80% of the total responders, we need to target upto 5th decile,
#or 50% of the prospects



# The average call duration for targeting the top X% prospects 

target_prospects<-prospect_df[1:nrow(prospect_df)*0.5,]
avg_target_duration<-mean(target_prospects$duration,na.rm = TRUE)
avg_target_duration
# The average call duration for targeting top 5 deciles of 
# prospects is 278.1231


# ------------------------------------------------------------------------


#-------------------------------------------------------
## 5. Creating lift charts
#-------------------------------------------------------


# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers.


# END ---------------------------------------------------------------------


