# Load SparkR

spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

# Load Libraries

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
library(stringr)
library(ggplot2)
library(dplyr)
# Initialise the sparkR session

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))


# Create a Spark DataFrame  

nyc_parking<- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",source = "csv", inferSchema = "true", header = "true")

# Examine the size

nrow(nyc_parking) #10803028

ncol(nyc_parking) #10

# Examine structure

str(nyc_parking)

# Check the Spark Dataframe

head(nyc_parking)

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 

sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

# Column Names contain spaces. We need to replace them with underscore.

colnames(nyc_parking)<- str_replace_all(colnames(nyc_parking), pattern=" ", replacement = "_")
nyc_parking$Issue_year<-year(nyc_parking$Issue_Date)
nyc_parking$Issue_month<-month(nyc_parking$Issue_Date)


# For using SQL, you need to create a temporary view

createOrReplaceTempView(nyc_parking, "nyc_parking_view")

Years<- SparkR::sql("SELECT Issue_Year, Count(*) as Number_of_Records from nyc_parking_view group by Issue_Year order by Number_of_Records desc")

head(Years,nrow(Years))

# There are 55 distinct years. As we have to consider data which belongs to 2017. We should consider only 2017.

nyc_parking<-nyc_parking[nyc_parking$Issue_year==2017]

dim(nyc_parking)
#5431918 rows and 12 columns

createOrReplaceTempView(nyc_parking, "nyc_parking_view")

#Showing distribution 
Distribution_on_years<- SparkR::sql("SELECT Issue_year,
                                    Issue_month,
                                    count(*) as Ticket_Frequency
                                    FROM nyc_parking_view
                                    GROUP BY Issue_year,
                                    Issue_month
                                    order by Ticket_Frequency desc")

Number_of_Violations_by_month<-head(Distribution_on_years,nrow(Distribution_on_years))
# Maximum number of violations are in the month of May. It has been observed that from July to December, there is a significant drop in number of violations.

ggplot(Number_of_Violations_by_month, aes(x=as.factor(Issue_month), y=Ticket_Frequency))+ geom_col() + xlab("Months") + ylab("Ticket Frequency") + ggtitle("Violations on the basis of month in 2017") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

Checking_null_values<-SparkR::sql("Select count(*) as Number_of_Null_Values from nyc_parking_view 
                                  where Summons_Number is NULL 
                                  or Plate_ID is NULL 
                                  or Registration_State is NULL 
                                  or Issue_Date is NULL
                                  or Violation_Code is NULL 
                                  or Vehicle_Body_Type is NULL
                                  or Vehicle_Make is NULL 
                                  or Violation_Precinct is NULL
                                  or Issuer_Precinct is NUll
                                  or Violation_Time is NULL ")
head(Checking_null_values)
# There is no field with null value.

#Checking on Plate_ID field to know if there are cases with same plate id.

Plate_Id_Check<-SparkR::sql("Select Plate_ID, count(*) as Ticket_Frequency from nyc_parking_view group by Plate_ID having count(*)>1 order by Ticket_Frequency desc")
head(Plate_Id_Check)
# There are 1974356 duplicate rows.

#   Plate_ID         Number_Of_Violations                                               
#1 BLANKPLATE                 5261
#2    96089MA                  529
#3    96087MA                  528
#4    94905JA                  527
#5    14828MJ                  526
#6    85848MD                  512

# There is one value'BLANKPLATE' which we cannot track. Therefore, we can remove this.

nyc_parking<-nyc_parking[nyc_parking$Plate_ID!='BLANKPLATE']


dim(nyc_parking)
#5426657 rows and 12 columns

# For using SQL, you need to create a temporary view

createOrReplaceTempView(nyc_parking, "nyc_parking_view")

# Lets see number of violations above 500

Plate_Id_Above_500<-SparkR::sql("Select Plate_ID, count(*) as Ticket_Frequency from nyc_parking_view group by Plate_ID having count(*)>=500 order by Ticket_Frequency desc")
Number_of_Violations_By_PlateID<-data.frame(head(Plate_Id_Above_500,nrow(Plate_Id_Above_500)))


ggplot(Number_of_Violations_By_PlateID, aes(x=as.factor(Plate_ID), y=Ticket_Frequency))+ geom_col() + xlab("Plate_ID") + ylab("Ticket Frequency") + ggtitle("Plate Ids with more than 500 Number of Violations") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)
# There are 7 Plate ID with more than 500 violations.


# Examine the data

# 1. Find the total number of tickets for the year.

q1<-SparkR::sql("Select count(distinct(Summons_Number)) from nyc_parking_view")

head(q1)

# 5426657
# As number of distinct Summons_Number is equal to total number of rows in data frame.
# That means there are no duplicate Summons_Number.


# 2. Find out the number of unique states from where the cars that got parking tickets came from. 

q2<- SparkR::sql("SELECT Registration_State, Count(*) as Number_of_Records from nyc_parking_view group by Registration_State order by Number_of_Records desc")

head(q2,nrow(q2))

# There are 64 distinct values of Registration_State.
# There is a numeric entry '99' in the column which should be corrected. We need to replace it with the state having maximum entries.
# As maximum number of tickets are issued in NY, We will replace 99 by NY.

nyc_parking$Registration_State<- ifelse(nyc_parking$Registration_State  %in% "99", "NY", nyc_parking$Registration_State)

# Lets check again for number of distinct Registration_State.

q2<-SparkR::sql("SELECT Registration_State, Count(*) as Ticket_Frequency from nyc_parking_view group by Registration_State order by Ticket_Frequency desc")

Violations_by_Registration_State<-data.frame(head(q2,nrow(q2)))
View(Violations_by_Registration_State)
# 64 unique registration states with maximum number of violations in NY.

#By using SQL, create a temporary veiw:

createOrReplaceTempView(nyc_parking, "nyc_parking_view")

ggplot(Violations_by_Registration_State, aes(x=as.factor(Registration_State), y=Ticket_Frequency))+ geom_col() + xlab("Registration_State") + ylab("Ticket Frequency") + ggtitle("Violations on the basis of Registration State") 

# Aggregation tasks

# 1. How often does each violation code occur? Display the frequency of the top five violation codes.

q3<- SparkR::sql("SELECT Violation_Code, Count(*) as Ticket_Frequency from nyc_parking_view group by Violation_code order by Ticket_Frequency desc")

head(q3,5)

# There are 100 violation codes out of which Top 5 voilation codes are 21,36,38,14,20.
#      Violation_code       Frequency                                                    
#1               21           767740
#2               36           662765
#3               38           541526
#4               14           476405
#5               20           319439

Top_5_Violation_Codes<- data.frame(head(q3,5))
ggplot(Top_5_Violation_Codes, aes(x=as.factor(Violation_Code), y=Ticket_Frequency))+ geom_col() + xlab("Violation code") + ylab("Ticket Frequency") + ggtitle("Top 5 Violation codes vs Ticket Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

# 2. How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 

vehicleBodyType<- SparkR::sql("SELECT Vehicle_Body_Type, count(*) as Ticket_Frequency
                              from nyc_parking_view 
                              group by Vehicle_Body_Type
                              order by Ticket_Frequency desc")
head(vehicleBodyType, 5)

#Vehicle_Body_Type Ticket_Frequency                                            
#1              SUBN          1882978
#2              4DSD          1547063
#3               VAN           723796
#4              DELV           358924
#5               SDN           192927

#Plotting the above records:
vehicleBodyType_top_5<- data.frame(head(vehicleBodyType,5))
ggplot(vehicleBodyType_top_5, aes(x=as.factor(Vehicle_Body_Type), y=Ticket_Frequency))+ geom_col() + xlab("Vehicle Body Type") + ylab("Ticket Frequency") + ggtitle("Top 5 Vehicle Body Type vs Ticket Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)


#How about the 'vehicle make'? 
vehicleMake<- SparkR::sql("SELECT Vehicle_Make, count(*) as Ticket_Frequency
                          from nyc_parking_view 
                          group by Vehicle_Make
                          order by Ticket_Frequency desc")
head(vehicleMake, 5)

#Results:
#Vehicle_Make Ticket_Frequency                                                 
#1         FORD           636527
#2        TOYOT           605011
#3        HONDA           538460
#4        NISSA           461725
#5        CHEVR           355868

vehicleMake_Top5<- data.frame(head(vehicleMake, 5))


ggplot(vehicleMake_Top5, aes(x=as.factor(Vehicle_Make), y=Ticket_Frequency))+ geom_col() + xlab("Vehicle Make") + ylab("Ticket Frequency") + ggtitle("Top 5 Vehicle Make vs Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

# Q 3 :  A precinct is a police station that has a certain zone of the city under its command. 
#Find the (5 highest) frequency of tickets for each of the following:

#1.'Violation Precinct' (this is the precinct of the zone where the violation occurred). Using
#this, can you make any insights for parking violations in any specific areas of the city?

Violation_Precinct<- SparkR::sql("SELECT Violation_Precinct, count(*) as Ticket_Frequency
                                 from nyc_parking_view 
                                 group by Violation_Precinct
                                 order by Ticket_Frequency desc")
head(Violation_Precinct,5)

#Violation_Precinct Ticket_Frequency                                           
#1                  0          925395
#2                 19           274264
#3                 14           203375
#4                  1           174620
#5                 18           169043

Violation_Precinct_Top5<- data.frame(head(Violation_Precinct,5))

ggplot(Violation_Precinct_Top5, aes(x=as.factor(Violation_Precinct), y=Ticket_Frequency))+ geom_col() + xlab("Violation Precinct") + ylab("Ticket_Frequency") + ggtitle("Top 5 Violation Precinct vs Ticket_Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

#2 'Issuer Precinct' (this is the precinct that issued the ticket)
Issue_precinct<- SparkR::sql("SELECT Issuer_Precinct, count(*) as Ticket_Frequency
                             from nyc_parking_view 
                             group by Issuer_Precinct
                             order by Ticket_Frequency desc")   
head(Issue_precinct,5)
#  Issuer_Precinct Ticket_Frequency                                              
#1               0          1077884
#2              19           266790
#3              14           200328
#4               1           168630
#5              18           162908

Issue_precinct_top5<- data.frame(head(Issue_precinct,5))

ggplot(Issue_precinct_top5, aes(x=as.factor(Issuer_Precinct), y=Ticket_Frequency))+ geom_col() + xlab("Issuer Precinct") + ylab("Ticket_Frequency") + ggtitle("Top 5 Issuer Precinct vs Ticket_Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

#Here you would have noticed that the dataframe has 'Violating Precinct' or 'Issuing Precinct'
#as '0'. These are the erroneous entries. Hence,provide the record for five correct precincts. 
#(Hint: Print top six entries after sorting)

head(Violation_Precinct,6)
#Violation_Precinct Ticket_Frequency                                           
#1                  0          925395
#2                 19           274264
#3                 14           203375
#4                  1           174620
#5                 18           169043
#6                114           147223

Violation_Precinct_Top6 <- data.frame(head(Violation_Precinct,6))
ggplot(Violation_Precinct_Top6, aes(x=as.factor(Violation_Precinct), y=Ticket_Frequency))+ geom_col() + xlab("Violation Precinct") + ylab("Ticket_Frequency") + ggtitle("Top 6 Violation Precinct vs Ticket_Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)


head(Issue_precinct,6)

#Issuer_Precinct Ticket_Frequency                                              
#1               0          1077884
#2              19           266790
#3              14           200328
#4               1           168630
#5              18           162908
#6             114           143900

Issue_precinct_top6<- data.frame(head(Issue_precinct,6))
ggplot(Issue_precinct_top6, aes(x=as.factor(Issuer_Precinct), y=Ticket_Frequency))+ geom_col() + xlab("Issuer Precinct") + ylab("Ticket_Frequency") + ggtitle("Top 6 Issuer Precinct vs Ticket_Frequency") + geom_text(aes(label=Ticket_Frequency),vjust=-0.3)

# ------------------------------------------------------------------------


# 4. Find the violation code frequency across three precincts which have issued the most number of tickets - 
# do these precinct zones have an exceptionally high frequency of certain violation codes?
# Are these codes common across precincts? 
# Hint: In the SQL view, use the 'where' attribute to filter among three precincts.

# 4.1 Finding violation code frequency

violation_code_freq<-SparkR::sql("select Issuer_Precinct,Violation_Code, count(*) as Frequency 
                                 from nyc_parking_view
                                 group by Issuer_Precinct, Violation_Code 
                                 order by Frequency desc" )

head(violation_code_freq,5)

#Issuer_Precinct Violation_Code Frequency                                      
#1               0             36    662765
#2               0              7    210175
#3               0             21    125923
#4              18             14     50135
#5              19             46     48422
#6               0              5     48076
#7              14             14     45019

# We are not considering 0. Therefore 18,19,14 are the three issuer precincts with maximum number of violations.


# 4.2 Common codes across precincts

common_codes<- SparkR::sql("select Violation_Code, count(*) as Frequency 
                           from nyc_parking_view
                           where Issuer_Precinct in (18,19,14) group by Violation_Code 
                           order by Frequency desc")

head(common_codes,5)

#Violation_Code Frequency                                                      
#1             14    124945
#2             46     63958
#3             69     53549
#4             38     45721
#5             37     39537

# 4.3 Exceptionally high frequencies of violation codes

violation_code_issuer_precinct_obj<-data.frame(head(common_codes))

ggplot(violation_code_issuer_precinct_obj,aes(x=as.factor(Violation_Code),y=Frequency))+geom_point()+xlab("Violation Code")+ylab("Frequency")+ggtitle("High Frequencies of violation code")

# Hence from plot, violation code 14 and 46 are exceptionally high

# ------------------------------------------------------------------------


# 5. Youâd want to find out the properties of parking violations across different times of the day:

# Find a way to deal with missing values, if any.
# Check for the null values using 'isNull' under the SQL. Also, to remove the null values, check the 'dropna' command in the API documentation.
# The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
# Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the three most commonly occurring violations.
# Hint: Use the CASE-WHEN in SQL view to segregate into bins. For finding the most commonly occurring violations, a similar approach can be used as mention in the hint for question 4.
# Now, try another direction. For the three most commonly occurring violation codes, find the most common time of the day (in terms of the bins from the previous part)

head(SparkR::sql("select count(*) from nyc_parking_view where Violation_Time is null or length(Violation_Time)<>5 or upper(substr(Violation_Time,-1)) not in ('A','P') or substr(Violation_Time,1,2) not in ('00','01','02','03','04','05','06','07','08','09','10','11','12')"))

bins<-SparkR::sql("SELECT Summons_Number, Violation_Code , case when substring(Violation_Time,1,2) in ('00','01','02','03','12') and upper(substring(Violation_Time,-1))='A' then 1 
                  when substring(Violation_Time,1,2) in ('04','05','06','07') and upper(substring(Violation_Time,-1))='A' then 2
                  when substring(Violation_Time,1,2) in ('08','09','10','11') and upper(substring(Violation_Time,-1))='A' then 3
                  when substring(Violation_Time,1,2) in ('12','00','01','02','03' ) and upper(substring(Violation_Time,-1))='P' then 4
                  when substring(Violation_Time,1,2) in ('04','05','06','07') and upper(substring(Violation_Time,-1))='P' then 5
                  when substring(Violation_Time,1,2) in ('08','09','10','11') and upper(substring(Violation_Time,-1))='P' then 6
                  else null end as Violation_Time_bin from nyc_parking_view where Violation_Time is not null or (length(Violation_Time)=5 and 
                  upper(substring(Violation_Time,-1)) in ('A','P') and substring(Violation_Time,1,2) in ('00','01','02','03','04','05','06','07', '08','09','10','11','12'))")

createOrReplaceTempView(bins, "bins_tbl")

violation_code_time_count <- SparkR::sql("SELECT Violation_Code,Violation_Time_bin, count(*) count from bins_tbl 
                                         group by Violation_Code,Violation_Time_bin")

violation_code_time_count_obj <- SparkR::collect(violation_code_time_count)


# Bin 1

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==1,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin count
#1             21                  1 36934
#2             40                  1 25810
#3             78                  1 15513

# Bin 2

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==2,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin count
#1             14                  2 74095
#2             40                  2 60609
#3             21                  2 57876


# Bin 3

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==3,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin  count
#1             21                  3 597827
#2             36                  3 348165
#3             38                  3 176310


# Bin 4

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==4,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin  count
#1             36                  4 286284
#2             38                  4 240511
#3             37                  4 166969


# Bin 5

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==5,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin  count
#1             38                  5 102784
#2             14                  5  75856
#3             37                  5  70317


# Bin 6 

arrange(violation_code_time_count_obj[violation_code_time_count_obj$Violation_Time_bin==6,], desc(count)) %>% head(n=3)

#Violation_Code Violation_Time_bin count
#1              7                  6 26293
#2             40                  6 22301
#3             14                  6 21026

# ------------------------------------------------------------------------

#Q6
# a)First, divide the year into some number of seasons, 
#   and find frequencies of tickets for each season.

Distribution_on_season<- SparkR::sql("SELECT Issue_month,violation_code,count(*) as Ticket_Frequency FROM nyc_parking_view  GROUP BY Issue_month,violation_code order by Ticket_Frequency desc")
Distribution_on_season$season<-ifelse(Distribution_on_season$Issue_month<=6,"S1","S2")
#Divided into two season based on number of violation S1 1st 6 months with  high violations.
# S2 last 6 months with  low violations.
# b)find the three most common violations for each of these seasons.

 Distribution_on_S1<-where(Distribution_on_season,Distribution_on_season$season=="S1")
 head(Distribution_on_S1,3)
# Top 3 violation for season-1

Distribution_on_S2<-where(Distribution_on_season,Distribution_on_season$season=="S2")
 head(Distribution_on_S2,3)
# Top 3 violation for season-2

#Q7

# a)Find total occurrences of the three most common violation codes
common_Violation<- SparkR::sql("select Violation_Code, count(*) as Frequency from nyc_parking_view group by Violation_Code order by Frequency desc")
 head(common_Violation,3)
#    Violation_Code Frequency                                                  
#1             21    767740   
#2             36    662765   
#3             38    541526   

#Using this information, find the total amount collected for the three violation codes with maximum tickets. State the code which has the highest total collection.
 common_Violation$fine<-ifelse(common_Violation$Violation_Code==21,common_Violation$frequency*55,common_Violation$frequency*50)
 head(common_Violation,3)
#    Violation_Code Frequency     fine                                             
#1             21    767740   42225700
#2             36    662765   33138250
#3             38    541526   27076300
# code with 21 had the highest collection.

#What can you intuitively infer from these findings?
#Jan to June had the major violation & July  to Dec has a drastic drop.
#Highest violation &collection was by Code-21(No parking where parking is not allowed by sign, street marking or traffic control device.)

sparkR.stop() 


