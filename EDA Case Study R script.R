## Exploratory Data Analysis group case study assimilated code
## Group Name: SCOOBY DATA DOO (PGDDS June'18 Cohort)
## Group Members: Siva Subramaniam Padmanaban (Group facilitator)
##                Shruti Saxena Das
##                Chinmay Sahu
##                Shailesh Kumar Jha


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------




# Loading the required packages

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
#library(ggthemes)
#rm(list=ls())

# Loading the data set 

loan_data<-read.csv("loan.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))




# ------------------------------------------------------------------------
# Data Understanding and Preparation --------------------------------------


# 1
summary(loan_data)
## As most of the constraints are blimful with NA's or have only one level of data hence they ought not to be considered 
#for further analysis

# 2
cols<-c(1:17,20:35,37:49,106)
loan_data<-loan_data[,cols]

# 3 Standardising Date Formats

loan_data$issue_d<-paste("01-",loan_data$issue_d,sep = "")
loan_data$issue_d<-as.Date(loan_data$issue_d,format = "%d-%b-%y")

loan_data$last_pymnt_d<-paste("01-",loan_data$last_pymnt_d,sep="")
loan_data$last_pymnt_d<-as.Date(loan_data$last_pymnt_d,format = "%d-%b-%y")

# 4 Standardising other textual constraints

loan_data$int_rate<-as.numeric(gsub("\\%","",loan_data$int_rate))
loan_data$annual_inc<-round(loan_data$annual_inc)

# 5 Standardising quantitative variable by removing outliers

quantile(loan_data$annual_inc,seq(0,1,0.05))
loan_data<-filter(loan_data,loan_data$annual_inc<=111000,loan_data$annual_inc>=30000)

# 6 Creating type driven metrics

loan_data$issue_d_month<-format(loan_data$issue_d,"%m")
loan_data$issue_d_year<-format(loan_data$issue_d,"%y")

# 7 As per the business understanding considering only the "Charged Off" and "Fully Paid" loans

master_loan<-filter(loan_data,loan_data$loan_status!="Current")





# ------------------------------------------------------------------------
# Univariate Analysis on Quantitative Variables ---------------------------


# Plot 1 : Univariate Analysis of Annual Income

annual_inc_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=annual_inc))
annual_inc_def<-annual_inc_def_obj+geom_histogram(binwidth = 10000,color="black",fill="lightsteelblue")+
  stat_bin(binwidth = 10000,geom="text",color="black",aes(label=..count..),
           position = position_stack(vjust = 0.5),size=3)+
  scale_x_continuous(breaks = seq(30000,111000,10000))+
  xlab("Annual Income")+ylab("Frequency Count")+
  ggtitle("PLOT 1.1: ANNUAL INCOME HISTOGRAM ( Defaulter )")+labs(fill="Loan Status")
annual_inc_def+theme(axis.text.x = element_text(angle = 90,hjust = 1))+theme()

annual_inc_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=annual_inc))
annual_inc_nondef<-annual_inc_nondef_obj+geom_histogram(binwidth = 10000,color="black",fill="lightsteelblue")+stat_bin(binwidth = 10000,geom="text",color="black",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+scale_x_continuous(breaks = seq(30000,111000,10000))+xlab("Annual Income")+ylab("Frequency Count")+ggtitle("PLOT 1.2: ANNUAL INCOME HISTOGRAM ( Non Defaulters )")+labs(fill="Loan Status")
annual_inc_nondef+theme(axis.text.x = element_text(angle = 90,hjust = 1))

# Plot 2: Univariate Analysis of Loan Amount

loan_amount_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=loan_amnt))
loan_amount_def<-loan_amount_def_obj+geom_histogram(binwidth=5000,color="black",fill="mistyrose")+stat_bin(binwidth = 5000,geom = "text",color="black",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+scale_x_continuous(breaks = seq(0,max(master_loan$funded_amnt),5000))+xlab("Funded Amount")+ylab("Frequency Count")+ggtitle("PLOT 2.1: FUNDED AMOUNT HISTOGRAM ( Defaulters ) ")
loan_amount_def+theme(axis.text.x = element_text(angle = 90,hjust=1))

loan_amount_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=loan_amnt))
loan_amount_nondef<-loan_amount_nondef_obj+geom_histogram(binwidth=5000,color="black",fill="mistyrose")+stat_bin(binwidth = 5000,geom = "text",color="black",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+scale_x_continuous(breaks = seq(0,max(master_loan$funded_amnt),5000))+xlab("Funded Amount")+ylab("Frequency Count")+ggtitle("PLOT 2.2: FUNDED AMOUNT HISTOGRAM ( Non Defaulters )")
loan_amount_nondef+theme(axis.text.x = element_text(angle = 90,hjust=1))

# Plot 3: Univariate Analysis on the number of open credit lines in borrower's credit file

open_acc_def_object<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=open_acc))
open_acc_def<-open_acc_def_object+geom_histogram(binwidth = 1,color="black",fill="plum2")+geom_freqpoly(binwidth=1,color="lightsalmon")+xlab("Credit Lines open in borrower's credit file")+ylab("Frequency Count")+ggtitle("PLOT 3.1: CURRENT CREDIT LINES HISTOGRAM ( Defaulters )")
open_acc_def+theme(axis.text.x = element_text(angle=90,hjust=1))

open_acc_nondef_object<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=open_acc))
open_acc_nondef<-open_acc_nondef_object+geom_histogram(binwidth = 1,color="black",fill="plum2")+geom_freqpoly(binwidth=1,color="lightsalmon")+xlab("Credit Lines open in borrower's credit file")+ylab("Frequency Count")+ggtitle("PLOT 3.2: CURRENT CREDIT LINES HISTOGRAM ( Non Defaulters )")
open_acc_nondef+theme(axis.text.x = element_text(angle=90,hjust=1))

# Plot 4: Univariate Analysis on the total number of open credit lines currently in the borrower's credit file

total_acc_def_object<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=total_acc))
total_acc_def<-total_acc_def_object+geom_histogram(binwidth = 1,col="black",fill="honeydew")+geom_freqpoly(binwidth=1,color="lightsalmon")+xlab("Credit Lines currently in borrower's credit file")+ylab("Frequency Count")+ggtitle("PLOT 4.1: TOTAL CREDIT LINES HISTOGRAM ( Defaulters ) ")
total_acc_def+theme(axis.text.x = element_text(angle = 90,hjust = 1))

total_acc_nondef_object<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=total_acc))
total_acc_nondef<-total_acc_nondef_object+geom_histogram(binwidth = 1,col="black",fill="honeydew")+geom_freqpoly(binwidth=1,color="lightsalmon")+xlab("Credit Lines currently in borrower's credit file")+ylab("Frequency Count")+ggtitle("PLOT 4.2: TOTAL CREDIT LINES HISTOGRAM ( Non Defaulters ) ")
total_acc_nondef+theme(axis.text.x = element_text(angle = 90,hjust = 1))


# Plot 5: Univariate Analysis on Interest Rate

int_rate_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=int_rate))
int_rate_def<-int_rate_def_obj+geom_histogram(binwidth = 1,color="black",fill="paleturquoise")+xlab("Interest Rate")+ylab("Count")+ggtitle("PLOT 5.1: INTEREST RATE HISTOGRAM")
int_rate_def

int_rate_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=int_rate))
int_rate_nondef<-int_rate_nondef_obj+geom_histogram(binwidth = 1,color="black",fill="paleturquoise")+xlab("Interest Rate")+ylab("Count")+ggtitle("PLOT 5.2: INTEREST RATE HISTOGRAM ( Non Defaulters) ")
int_rate_nondef


# Plot 6: Univariate Analysis on DTI

dti_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=dti))
dti_def<-dti_def_obj+geom_histogram(binwidth = 1,color="black",fill="lavender")+xlab(" DTI Ratio")+ylab("Count")+ggtitle("PLOT 6.1: DTI HISTOGRAM ( Defaulters )")
dti_def

dti_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=dti))
dti_nondef<-dti_nondef_obj+geom_histogram(binwidth = 1,color="black",fill="lavender")+xlab(" DTI Ratio")+ylab("Count")+ggtitle("PLOT 6.2: DTI HISTOGRAM ( Non Defaulters )")
dti_nondef




# ------------------------------------------------------------------------
# Univariate Analysis on Ordered Categorical Variables --------------------



# Plot 7: Univariate Analysis on the Grade

grade_defaulted_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=grade))
grade_defaulted<-grade_defaulted_obj+geom_bar(fill="slategray2")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Grade")+ylab("Count")+ggtitle("PLOT 7.1: GRADE DISTRIBUTION ( Defaulters )")
grade_defaulted

grade_non_defaulters_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=grade))
grade_non_defaulters<-grade_non_defaulters_obj+geom_bar(fill="slategray2")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Grade")+ylab("Count")+ggtitle("PLOT 7.2: GRADE DISTRIBUTION ( Non Defaulters )")
grade_non_defaulters

# Plot 8: Univariate Analysis on Emp_length

emp_length_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=emp_length))
emp_length_def<-emp_length_def_obj+geom_bar(fill="peachpuff3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Emp Length")+ylab("Count")+ggtitle("PLOT 8.1: EMPLOYEE LENGTH DISTRIBUTION ( Defaulters )")
emp_length_def+theme(axis.text.x = element_text(angle = 90,hjust = 1))

emp_length_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=emp_length))
emp_length_nondef<-emp_length_nondef_obj+geom_bar(fill="peachpuff3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Emp Length")+ylab("Count")+ggtitle("PLOT 8.2 EMPLOYEE LENGTH DISTRIBUTION ( Non Defaulters )")
emp_length_nondef+theme(axis.text.x = element_text(angle = 90,hjust=1))


# Plot 9: Univariate Analysis on Home Ownership

home_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=home_ownership))
home_def<-home_def_obj+geom_bar(fill="mediumseagreen")+xlab("Home Ownership")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+ylab("Count")+ggtitle("PLOT 9.1: HOME OWNERSHIP DISTRIBUTION ( Defaulters )")
home_def

home_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=home_ownership))
home_nondef<-home_nondef_obj+geom_bar(fill="mediumseagreen")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Home Ownership")+ylab("Count")+ggtitle("PLOT 9.2: HOME OWNERSHIP DISTRIBUTION ( Non Defaulters )")
home_nondef


# Plot 10: Univariate Analysis on Verification Status

verification_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=verification_status))
verification_def<-verification_def_obj+geom_bar(fill="thistle3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Verification Status")+ylab("Count")+ggtitle("PLOT 10.1: VERIFICATION STATUS DISTRIBUTION ( Defaulters )")
verification_def

verification_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=verification_status))
verification_nondef<-verification_nondef_obj+geom_bar(fill="thistle3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Verification Status")+ylab("Count")+ggtitle("PLOT 10.2: VERIFICATION STATUS DISTRIBUTION ( Non Defaulters )")
verification_nondef


# Plot 11: Univariate Analysis on Public Record

public_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=pub_rec))
public_def<-public_def_obj+geom_bar(fill="sienna1")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Number Of Public Records")+ylab("Count")+ggtitle("PLOT 11.1: PUBLIC RECORD HISTOGRAM ( Defaulters )")
public_def

public_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=pub_rec))
public_nondef<-public_nondef_obj+geom_bar(fill="sienna1")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Number Of Public Records")+ylab("Count")+ggtitle("PLOT 11.2: PUBLIC RECORD HISTOGRAM ( Non Defaulters )")
public_nondef


# Plot 12: Univariate Analysis on Public Record Bancruptcy

master_loan_pub<-master_loan[!is.na(master_loan$pub_rec_bankruptcies),]

public_bankruptcy_def_obj<-ggplot(subset(master_loan_pub,master_loan_pub$loan_status=="Charged Off"),aes(x=pub_rec_bankruptcies))
public_bankruptcy_def<-public_bankruptcy_def_obj+geom_bar(fill="powderblue")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Number Of Public Record Bankrutpcies")+ylab("Count")+ggtitle("PLOT 12.1: PUBLIC RECORD BANKRUPTCY HISTOGRAM ( Defaulters )")
public_bankruptcy_def

public_bankruptcy_nondef_obj<-ggplot(subset(master_loan_pub,master_loan_pub$loan_status=="Fully Paid"),aes(x=pub_rec_bankruptcies))
public_bankruptcy_nondef<-public_bankruptcy_nondef_obj+geom_bar(fill="powderblue")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Number Of Public Record Bankrutpcies")+ylab("Count")+ggtitle("PLOT 12.2: PUBLIC RECORD BANKRUPTCY HISTOGRAM ( Non Defaulters )")
public_bankruptcy_nondef


# Plot 13: Univariate Analysis on Delinq_2yr

delinq_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=delinq_2yrs))
delinq_def<-delinq_def_obj+geom_bar(col="black",fill="wheat2")+xlab("Number Of Delinquency")+ylab("Count")+ggtitle("PLOT 13.1: 30+ DAYS DELINQUENCY DISTRIBUTION ( Defaulters )")
delinq_def

delinq_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=delinq_2yrs))
delinq_nondef<-delinq_nondef_obj+geom_bar(col="black",fill="wheat2")+xlab("Number Of Delinquency")+ylab("Count")+ggtitle("PLOT 13.2: 30+ DAYS DELINQUENCY DISTRIBUTION ( Non Defaulters )")
delinq_nondef


# Plot 14: Univariate Analysis on Purpose

purpose_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=purpose))
purpose_def<-purpose_def_obj+geom_bar(fill="steelblue1")+xlab("Purpose")+ylab("Count")+ggtitle("PLOT 14.1: PURPOSE DISTRIBUTION ( Defaulters )")
purpose_def+theme(axis.text.x = element_text(angle=90,hjust = 1))

purpose_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=purpose))
purpose_nondef<-purpose_nondef_obj+geom_bar(fill="steelblue1")+xlab("Purpose")+ylab("Count")+ggtitle("PLOT 14.2: PURPOSE DISTRIBUTION ( Non Defaulters )")
purpose_nondef+theme(axis.text.x = element_text(angle=90,hjust = 1))


# Plot 15: Univariate Analysis on Term

term_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(x=term))
term_def<-term_def_obj+geom_bar(fill="pink3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Term")+ylab("Count")+ggtitle("PLOT 14.1: TERM DISTRIBUTION ( Defaulters )")
term_def

term_nondef_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Fully Paid"),aes(x=term))
term_nondef<-term_nondef_obj+geom_bar(fill="pink3")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Term")+ylab("Count")+ggtitle("PLOT 14.1: TERM DISTRIBUTION ( Non Defaulters )")
term_nondef




# ------------------------------------------------------------------------
# Summary Metric ----------------------------------------------------------


# Plot 16: Univariate Boxplot on Annual Income segmented upon Loan Status

box_annual_inc_stat_obj<-ggplot(master_loan,aes(y=master_loan$annual_inc,fill=factor(master_loan$loan_status)))
box_annual_inc_stat<-box_annual_inc_stat_obj+geom_boxplot()+xlab("Annual Income")+ylab("Distribution")+labs(fill="Loan Status")+ggtitle("PLOT 16: ANNUAL INCOME BOXPLOT SEGMENTED ON LOAN STATUS")
box_annual_inc_stat



# ------------------------------------------------------------------------
# Bivariate Analysis ------------------------------------------------------


# Plot 17: Bivariate Analysis on Loan Issued per Month

loan_issue_obj<-ggplot(master_loan,aes(x=master_loan$issue_d_month,fill=factor(master_loan$loan_status)))
loan_issue<-loan_issue_obj+geom_bar(stat="count")+xlab("Month")+ylab("Count")+ggtitle("PLOT 17: MONTHLY LOAN ISSUE DISTRIBUTION")+labs(fill="Loan Status")+geom_text(aes(label=..count..),stat="count",position=position_stack(vjust=0.5))
loan_issue

# Plot 18: Bivariate Analysis on Loan Defaults per Month

loan_issue_def_obj<-ggplot(subset(master_loan,master_loan$loan_status=="Charged Off"),aes(issue_d_month))
loan_issue_def<-loan_issue_def_obj+geom_bar(stat="count",fill="tomato")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size=3)+xlab("Month")+ylab("Count")+ggtitle("PLOT 18: MONTHLY LOAN DEFAULT DISTRIBUTION")
loan_issue_def

# Plot 19: Bivariate Analysis on Verification based on Loan Status

ver_stat_obj<-ggplot(master_loan,aes(x=factor(master_loan$verification_status),fill=master_loan$loan_status))
ver_stat<-ver_stat_obj+geom_bar()+xlab("Verification Status")+ylab("Count")+ggtitle("PLOT 19: VERIFICATION vs STATUS ")+labs(fill="Loan Status")+geom_text(aes(label=..count..),stat="count",position=position_stack(vjust=0.5))
ver_stat+theme(axis.text.x = element_text(angle=90,hjust=1))

# Plot 20: Bivariate Analysis on Annual Income based upon Loan Status

grade_stat_obj<-ggplot(master_loan,aes(x=master_loan$grade,fill=master_loan$loan_status))
grade_stat<-grade_stat_obj+geom_bar()+xlab("Grade")+ylab("Count")+labs(fill="Loan Status")+ggtitle("PLOT 20: GRADE DISTRIBUTION BASED UPON LOAN STATUS")+geom_text(aes(label=..count..),stat="count",position=position_stack(vjust=0.5))
grade_stat


# Plot 21: Bivariate Analysis on Grade based upon Loan Status

inc_stat_obj<-ggplot(master_loan,aes(x=master_loan$annual_inc,fill=master_loan$loan_status))
inc_stat<-inc_stat_obj+geom_histogram(binwidth = 10000,color="black")+scale_x_continuous(breaks =seq(0,111000,10000))+xlab("Annual Income")+ylab("Count")+labs(fill="Loan Status")+ggtitle("PLOT 21: ANNUAL INCOME HISTOGRAM BASED UPON LOAN STATUS")
inc_stat+theme(axis.text.x = element_text(angle = 90,hjust = 1))


# Plot 22: Bivariate Analysis on DTI Ratio based upon Loan Status

dti_stat_obj<-ggplot(master_loan,aes(x=master_loan$dti,fill=factor(master_loan$loan_status)))
dti_stat<-dti_stat_obj+geom_histogram(binwidth = 1,color="black")+xlab("DTI")+ylab("Count")+labs(fill="Loan Status")+ggtitle("PLOT 22: DTI RATIO HISTOGRAM BASED UPON LOAN STATUS")
dti_stat


# Plot 23: Bivariate Analysis on Loan Issued per state

loan_issue_obj<-ggplot(master_loan,aes(x=master_loan$addr_state,fill=factor(master_loan$loan_status)))
loan_issue<-loan_issue_obj+geom_bar(stat="count")+xlab("Month")+ylab("Count")+ggtitle("PLOT 23: BIVARIATE ANALYSIS ON LOAN ISSUED PER STATE")+labs(fill="Loan Status")+geom_text(aes(label=..count..),stat="count",position=position_stack(vjust=0.5))
loan_issue



# ------------------------------------------------------------------------
# Segmented Bivariate Anlysis ---------------------------------------------



# Plot 23: Segmented Bivariate on Loan Issued for different Grades based upon Loan Status

loan_grade_def_group<-group_by(subset(master_loan,master_loan$loan_status=="Charged Off"),"Grade"=grade)
loan_grade_def<-summarise(loan_grade_def_group,"Amount"=sum(loan_amnt,na.rm = TRUE))
loan_grade_def

loan_grade_nondef_group<-group_by(subset(master_loan,master_loan$loan_status!="Charged Off"),"Grade"=grade)
loan_grade_nondef<-summarise(loan_grade_nondef_group,"Amount"=sum(loan_amnt,na.rm = TRUE))
loan_grade_nondef


loan_grade_stat_def_obj<-ggplot(loan_grade_def,aes(x=Grade,y=Amount))
loan_grade_stat_def<-loan_grade_stat_def_obj+geom_bar(stat = "identity",fill="lightgoldenrod",color="black")+xlab("Grade")+ylab("Loan Amount")+ggtitle("PLOT 23.1: LOAN AMOUNT GROUPED BY GRADE ( Defaulters )")+scale_y_continuous(labels = scales::comma)
loan_grade_stat_def

loan_grade_stat_nondef_obj<-ggplot(loan_grade_nondef,aes(x=Grade,y=Amount))
loan_grade_stat_nondef<-loan_grade_stat_nondef_obj+geom_bar(stat = "identity",fill="lightgoldenrod",color="black")+xlab("Grade")+ylab("Loan Amount")+ggtitle("PLOT 23.2: LOAN AMOUNT GROUPED BY GRADE ( Non Defaulters )")+scale_y_continuous(labels = scales::comma)
loan_grade_stat_nondef


# Plot 24: Segmented Bivariate on Loan Issued for different Verification Status based upon Loan Status

loan_veri_def_group<-group_by(subset(master_loan,master_loan$loan_status=="Charged Off"),"Verification_Status"=verification_status)
loan_veri_def<-summarise(loan_veri_def_group,"Amount"=sum(loan_amnt,na.rm = TRUE))
loan_veri_def

loan_veri_nondef_group<-group_by(subset(master_loan,master_loan$loan_status!="Charged Off"),"Verification_Status"=verification_status)
loan_veri_nondef<-summarise(loan_veri_nondef_group,"Amount"=sum(loan_amnt,na.rm = TRUE))
loan_veri_nondef

loan_veri_stat_def_obj<-ggplot(loan_veri_def,aes(x=Verification_Status,y= Amount))
loan_veri_stat_def<-loan_veri_stat_def_obj+geom_bar(stat = "identity",color="black",fill="skyblue")+xlab("Verification Status")+ylab("Loan Amount")+ggtitle("PLOT 24.1: LOAN AMOUNT GROUPED BY VERIFICATION STATUS ( Defaulters )")+scale_y_continuous(labels = scales::comma)
loan_veri_stat_def

loan_veri_stat_nondef_obj<-ggplot(loan_veri_nondef,aes(x=Verification_Status,y= Amount))
loan_veri_stat_nondef<-loan_veri_stat_nondef_obj+geom_bar(stat = "identity",color="black",fill="skyblue")+xlab("Verification Status")+ylab("Loan Amount")+ggtitle("PLOT 24.2: LOAN AMOUNT GROUPED BY VERIFICATION STATUS ( Non Defaulters )")+scale_y_continuous(labels = scales::comma)
loan_veri_stat_nondef




# ------------------------------------------------------------------------
# END ---------------------------------------------------------------------


