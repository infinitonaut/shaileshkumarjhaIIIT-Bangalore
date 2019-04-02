## Linear Regression Assignment- Geely Auto
## PGDDS June 2018 cohort
## Shailesh Kumar Jha

# -----------------------------------------------------------------------
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# 1. Business Understanding -----------------------------------------------


# 1.1 Geely Auto, a car manufacturing company wants to foray into US car
# market industry, by manufacturing the cars locally so they will be
# face competition from US and European counterparts.

# 1.2 For being a step ahead from their competititors they want to 
# know the factors on which the price of cars depends in the US market.
# Hence they awarded a contract to an automobile consulting firm to
# understand the factors on which the price of car depends.

# 1.3 Essentially they want to know- which variables are significant 
# in predicting and how well those variables describe the price of
# a car.

# 1.4 Based on market survey the consulting firm has gathered a large 
# dataset of different types of cars across the American Market.

# 1.5 The goal- to model the price of cars with the availabe independent
# variables. The model will be used by Gilly Auto to understand how 
# the prices vary with the covariates so that they manipulate
# the design of the car, the business strategy et cetera to meet 
# certain price levels. 

# 1.6 The model will be a good method to understand the pricing
# dynamics of a new market.




# ------------------------------------------------------------------------
# 2. Data Definition ------------------------------------------------------


# The data dictionary already provided is copied below to get an initial
# handle on the data set.

# 2.1	Car_ID				                    Unique id of each observation (Integer)		
# 2.2	Symboling 				                Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe. (Categorical) 		
# 2.3	carCompany				                Name of car company (Categorical)		
# 2.4	fueltype				                  Car fuel type i.e gas or diesel (Categorical)		
# 2.5	aspiration				                Aspiration used in a car (Categorical)		
# 2.6	doornumber				                Number of doors in a car (Categorical)		
# 2.7	carbody				                    Body of car (Categorical)		
# 2.8	drivewheel				                Type of drive wheel (Categorical)		
# 2.9	enginelocation		                Location of car engine (Categorical)		
# 2.10	wheelbase				                Wheelbase of car (Numeric)		
# 2.11	carlength				                Length of car (Numeric)		
# 2.12	carwidth				                Width of car (Numeric)		
# 2.13	carheight				                Height of car (Numeric)		
# 2.14	curbweight			                The weight of a car without occupants or baggage (Numeric)		
# 2.15	enginetype			                Type of engine (Categorical)		
# 2.16	cylindernumber	                Cylinder placed in the car (Categorical)		
# 2.17	enginesize			                Size of car (Numeric)		
# 2.18	fuelsystem			                Fuel system of car (Categorical)		
# 2.19	boreratio				                Bore ratio of car (Numeric)		
# 2.20	stroke				                  Stroke or volume inside the engine (Numeric)		
# 2.21	compressionratio                Compression ratio of car (Numeric)		
# 2.22	horsepower			                Horsepower (Numeric)		
# 2.23	peakrpm				                  Car peak rpm (Numeric)		
# 2.24	citympg				                  Mileage in city (Numeric)		
# 2.25	highwaympg		                  Mileage on highway (Numeric)		
# 2.26	price(Dependent variable)				Price of car (Numeric)		




# ------------------------------------------------------------------------
# 3. Sourcing Data -------------------------------------------------------


# 3.1 Loading data set into R environment 

geely_car<- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
str(geely_car)
# The data frame is comprises 205 observations of 26 variables

# 3.2 Loading the required libraries

library(car)
library(MASS)
library(stringr)
library(tidyr)
library(ggplot2)




# ------------------------------------------------------------------------
# 4. Cleaning the data ----------------------------------------------------


# 4.1 Fixing Rows- No unnecessary header and footer rows,
# no summary rows, no indiacator rows, no blank rows

# 4.2 Fixing Columns- No column name missing, no need for renaming
# columns, no misaligned columns, neither any need for merging identifiers,
# deleting car_ID which is unnecessary, splitting 'CarName' into 
# 'carcompany' & 'carmodel'

geely_car<- geely_car[,-1]

sourcing_error<-which(str_count(geely_car$CarName," ")==0)
geely_car<-geely_car[-sourcing_error,]
geely_car<-separate(geely_car,CarName,c("carcompany","carmodel"),sep=" ",extra = c("merge"))

# 4.3 Missing values- No missing values

sum(is.na(geely_car))

# 4.4 Will standardise quantitative variables in EDA, after manually
# sifting no invalid values are present and if any will do in EDA, 
# will filter the data if whenever required.




# ------------------------------------------------------------------------
# 5. Exploratory Data Analysis --------------------------------------------



## Univarite on categorical variables


# Plot 1: Distribution of 'symboling'

symboling_obj<-ggplot(geely_car,aes(x=factor(symboling)))
symboling_plot<-symboling_obj+geom_bar(color="black",fill="slategray2")+geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust = 0.5))+xlab("Risk Symboling")+ylab("Count")+ggtitle("PLOT 1: SYMBOLING DISTRIBUTION")
symboling_plot


# Plot 2: Distribution of 'carcompany'- By initial plot we comprehend 
# there are mistakes for 'mazda','porsche','toyota','volkswagen'.
# Hence after correct the quality issues we plot again.

carcompany_ini_obj<-ggplot(geely_car,aes(x=factor(carcompany)))
carcompany_ini_plot<-carcompany_ini_obj+geom_bar(color="black",fill="mistyrose")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust=0.5))+xlab("Car Company")+ylab("Count")+ggtitle("PLOT 2: CAR COMPANY DISTRIBUTION ( Initial )")
carcompany_ini_plot+theme(axis.text.x = element_text(angle=90,hjust = 1))

geely_car$carcompany<-gsub(pattern = "maxda",replacement = "mazda",geely_car$carcompany)
geely_car$carcompany<-gsub(pattern = "porcshce",replacement = "porsche",geely_car$carcompany)
geely_car$carcompany<-gsub(pattern = "toyouta",replacement = "toyota",geely_car$carcompany)
geely_car$carcompany<-gsub(pattern = "vokswagen",replacement = "volkswagen",geely_car$carcompany)
geely_car$carcompany<-gsub(pattern = "vw",replacement = "volkswagen",geely_car$carcompany)
geely_car$carcompany<-gsub(pattern = "Nissan",replacement = "nissan",geely_car$carcompany)
  
carcompany_obj<-ggplot(geely_car,aes(x=factor(carcompany)))
carcompany_plot<-carcompany_obj+geom_bar(color="black",fill="mistyrose")+geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+xlab("Car Company")+ylab("Count")+ggtitle("PLOT 2: COMPANY DISTRIBUTION")
carcompany_plot+theme(axis.text.x = element_text(angle=90,hjust = 1))


# Plot 3: Distribution of 'fueltype'

fueltype_obj<-ggplot(geely_car,aes(x=factor(fueltype)))
fueltype_plot<-fueltype_obj+geom_bar(color="black",fill="honeydew")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Fuel Type")+ylab("Count")+ggtitle("PLOT 3: FUEL TYPE DISTRIBUTION")
fueltype_plot


# Plot 4: Distribution of 'aspiration'

aspiration_obj<-ggplot(geely_car,aes(x=factor(aspiration)))
aspiration_plot<-aspiration_obj+geom_bar(color="black",fill="paleturquoise")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Aspiration")+ylab("Count")+ggtitle("PLOT 4: ASPIRATION DISTRIBUTION")
aspiration_plot


# Plot 5: Distribution of 'doornumber'

doornumber_obj<-ggplot(geely_car,aes(x=factor(doornumber)))
doornumber_plot<-doornumber_obj+geom_bar(color="black",fill="lavender")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Door Number")+ylab("Count")+ggtitle("PLOT 5: DOOR NUMBER DISTRIBUTION")
doornumber_plot


# Plot 6: Distribution of 'carbody'

carbody_obj<-ggplot(geely_car,aes(x=factor(carbody)))
carbody_plot<-carbody_obj+geom_bar(color="black",fill="peachpuff3")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Car Body")+ylab("Count")+ggtitle("PLOT 6: CAR BODY DISTRIBUTION")
carbody_plot


# Plot 7: Distribution of 'drivewheel'

drivewheel_obj<-ggplot(geely_car,aes(x=factor(drivewheel)))
drivewheel_plot<-drivewheel_obj+geom_bar(color="black",fill="mediumseagreen")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Type of Drivewheel")+ylab("Count")+ggtitle("PLOT 7: DRIVEWHEEL DISTRIBUTION")
drivewheel_plot


# Plot 8: Distribution of 'enginelocation'

enginelocation_obj<-ggplot(geely_car,aes(x=factor(enginelocation)))
enginelocation_plot<-enginelocation_obj+geom_bar(color="black",fill="thistle3")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Location of Engine")+ylab("Count")+ggtitle("PLOT 8: ENGINE LOCATION DISTRIBUTION")
enginelocation_plot


# Plot 9: Distribution of 'enginetype'- dohcv is 1 in number hence better
# to remove the data entry

enginetype_obj<-ggplot(geely_car,aes(x=factor(enginetype)))
enginetype_plot<-enginetype_obj+geom_bar(color="black",fill="sienna1")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Type of Engine")+ylab("Count")+ggtitle("PLOT 9: ENGINE TYPE DISTRIBUTION")
enginetype_plot

dohcv<-which(geely_car$enginetype=='dohcv')
dohcv
geely_car<-geely_car[-dohcv,]


# Plot 10: Distribution of 'cylindernumber'

cylindernumber_obj<-ggplot(geely_car,aes(x=factor(cylindernumber)))
cylindernumber_plot<-cylindernumber_obj+geom_bar(color="black",fill="powderblue")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Number of Cylinders")+ylab("Count")+ggtitle("PLOT 10: CYLINDER NUMBER DISTRIBUTION")
cylindernumber_plot


# Plot 11: Distribution of 'fuelsystem'- Count of 'mfi' and 'spfi'
# hence better to remove

fuelsystem_obj<-ggplot(geely_car,aes(x=factor(fuelsystem)))
fuelsystem_plot<-fuelsystem_obj+geom_bar(color="black",fill="wheat2")+geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+xlab("Fuel System")+ylab("Count")+ggtitle("PLOT 11: FUEL SYSTEM DISTRIBUTION")
fuelsystem_plot

mfi<-which(geely_car$fuelsystem=='mfi')
mfi
spfi<-which(geely_car$fuelsystem=='spfi')
spfi
geely_car<-geely_car[-mfi,]
geely_car<-geely_car[-spfi,]



## Univariate on quantitative variables


# Plot 12: Box Plot of 'wheelbase'

wheelbase_obj<-ggplot(geely_car,aes(y=wheelbase))
wheelbase_plot<-wheelbase_obj+geom_boxplot(fill="steelblue1")+ylab("Wheel base")+ggtitle("PLOT 12: WHEELBASE BOXPLOT")
wheelbase_plot
# The outliers are clearly visible in the plot therefore capping
# at the required percentile

quantile(geely_car$wheelbase,seq(0,1,0.01))
geely_car$wheelbase[which(geely_car$wheelbase>115.600)]<-115.600


# Plot 13: Box Plot of 'carlength'

carlength_obj<-ggplot(geely_car,aes(y=carlength))
carlength_plot<-carlength_obj+geom_boxplot(fill="pink3")+ylab("Car Length")+ggtitle("PLOT 13: CAR LENGTH BOXPLOT")
carlength_plot


# Plot 14: Box Plot of 'carwidth'

carwidth_obj<-ggplot(geely_car,aes(y=carwidth))
carwidth_plot<-carwidth_obj+geom_boxplot(fill="lightsteelblue")+ylab("Car Width")+ggtitle("PLOT 14: CAR WIDTH BOXPLOT")
carwidth_plot
# The outliers are clearly visible in the plot therefore capping
# at the required percentile

quantile(geely_car$carwidth,seq(0,1,0.01))
geely_car$carwidth[which(geely_car$carwidth>70.612)]<-70.612


# Plot 15: Box Plot of 'carheight'

carheight_obj<-ggplot(geely_car,aes(y=carheight))
carheight_plot<-carheight_obj+geom_boxplot(fill="mistyrose")+ylab("Car Height")+ggtitle("PLOT 15: CAR HEIGHT BOXPLOT")
carheight_plot


# Plot 16: Box Plot of 'curbweight'

curbweight_obj<-ggplot(geely_car,aes(y=curbweight))
curbweight_plot<-curbweight_obj+geom_boxplot(fill="plum2")+ylab("Curb Weight")+ggtitle("PLOT 16: CURB WEIGHT BOXPLOT")
curbweight_plot


# Plot 17: Box Plot of 'enginesize'

enginesize_obj<-ggplot(geely_car,aes(y=enginesize))
enginesize_plot<-enginesize_obj+geom_boxplot(fill="honeydew")+ylab("Engine Size")+ggtitle("PLOT 17: ENGINE SIZE BOXPLOT")
enginesize_plot


# Plot 18: Box Plot of 'boreratio'

boreratio_obj<-ggplot(geely_car,aes(y=boreratio))
boreratio_plot<-boreratio_obj+geom_boxplot(fill="lavender")+ylab("Bore Ratio")+ggtitle("PLOT 18: BORE RATIO BOXPLOT")
boreratio_plot


# Plot 19: Box Plot of 'stroke'

stroke_obj<-ggplot(geely_car,aes(y=stroke))
stroke_plot<-stroke_obj+geom_boxplot(fill="slategray2")+ylab("Stroke")+ggtitle("PLOT 19: STROKE BOXPLOT")
stroke_plot
# The visible outliers in the plot are treated by capping at the
# required percentile

quantile(geely_car$stroke,seq(0,1,0.01))
geely_car$stroke[which(geely_car$stroke>3.8600)]<-3.8600
geely_car$stroke[which(geely_car$stroke<2.6400)]<-2.6400


# Plot 20: Box Plot of 'compressionratio'

compressionratio_obj<-ggplot(geely_car,aes(y=compressionratio))
compressionratio_plot<-compressionratio_obj+geom_boxplot(fill="peachpuff3")+ylab("Compression Ratio")+ggtitle("PLOT 20: COMPRESSION RATIO BOXPLOT")
compressionratio_plot
# The visible outliers are treated by capping at the required
# percentile

quantile(geely_car$compressionratio,seq(0,1,0.01))
geely_car$compressionratio[which(geely_car$compressionratio>10.0110)]<-10.0110
geely_car$compressionratio[which(geely_car$compressionratio<7.4850)]<-7.4850


# Plot 21: Box Plot of 'horsepower'

horsepower_obj<-ggplot(geely_car,aes(y=horsepower))
horsepower_plot<-horsepower_obj+geom_boxplot(fill="thistle3")+ylab("Horsepower")+ggtitle("PLOT 21: HORSE POWER BOXPLOT")
horsepower_plot
# Treating the visible outliers at the required percentile

quantile(geely_car$horsepower,seq(0,1,0.01))
geely_car$horsepower[which(geely_car$horsepower>184.00)]<-184.00


# Plot 22: Box Plot of 'peakrpm'

peakrpm_obj<-ggplot(geely_car,aes(y=peakrpm))
peakrpm_plot<-peakrpm_obj+geom_boxplot(fill="sienna1")+ylab("Peak RPM")+ggtitle("PLOT 22: PEAK RPM BOXPLOT")
peakrpm_plot
# Treating the outlier by capping at the required percentile

quantile(geely_car$peakrpm,seq(0,1,0.01))
geely_car$peakrpm[which(geely_car$peakrpm>6006.0)]<-6006.0


# Plot 23: Box Plot of 'citympg'

citympg_obj<-ggplot(geely_car,aes(y=citympg))
citympg_plot<-citympg_obj+geom_boxplot(fill="powderblue")+ylab("City Mileage mpg")+ggtitle("PLOT 23: CITY MILEAGE BOXPLOT")
citympg_plot
# Treating the visible outlier by capping  at the required percentile

quantile(geely_car$citympg,seq(0,1,0.01))
geely_car$citympg[which(geely_car$citympg>38.00)]<-38.00


# Plot 24: Box Plot of 'highwaympg'

highwaympg_obj<-ggplot(geely_car,aes(y=highwaympg))
highwaympg_plot<-highwaympg_obj+geom_boxplot(fill="wheat2")+ylab("Highway Mileage mpg")+ggtitle("PLOT 24: HIGHWAY MILEAGE BOXPLOT")
highwaympg_plot
# Treating the visible ouliers by capping

quantile(geely_car$highwaympg,seq(0,1,0.01))
geely_car$highwaympg[which(geely_car$highwaympg>46.00)]<-46.00


# Plot 25: Box Plot of 'price' which is the dependent variable

price_obj<-ggplot(geely_car,aes(y=price))
price_plot<-price_obj+geom_boxplot(fill="steelblue1")+ylab("Price Observed")+ggtitle("PLOT 24: OBSERVED PRICE BOXPLOT")
price_plot
# The outliers having significant difference between them are treated
# by capping at ther required percentile

quantile(geely_car$price,seq(0,1,0.01))
geely_car$price[which(geely_car$price>36882.960)]<-36882.960




# ------------------------------------------------------------------------
# 6. Dummy Variables Creation ---------------------------------------------
# To deal with categorical data while building the regression model



# 6.1 Dummy Variable for 'symboling'

levels(factor(geely_car$symboling))
geely_car$symboling<-as.factor(geely_car$symboling)

dummy_symboling<-data.frame(model.matrix(~symboling,data = geely_car))
dummy_symboling<-dummy_symboling[,-1]

geely_car<-geely_car[,-1]
geely_car<-cbind(geely_car,dummy_symboling)
rm(dummy_symboling)


# 6.2 Dummy Variable for 'carcompany'

levels(factor(geely_car$carcompany))
geely_car$carcompany<-as.factor(geely_car$carcompany)

dummy_carcompany<-data.frame(model.matrix(~carcompany,data=geely_car))
dummy_carcompany<-dummy_carcompany[,-1]

geely_car<-geely_car[,-1]
geely_car<-cbind(geely_car,dummy_carcompany)
rm(dummy_carcompany)


# 6.3 Dummy Variable for 'fueltype'

geely_car$fueltype<-as.factor(geely_car$fueltype)
levels(geely_car$fueltype)
levels(geely_car$fueltype)<-c(1,0)
geely_car$fueltype<-as.numeric(levels(geely_car$fueltype))[geely_car$fueltype]


# 6.4 Dummy Variable for 'aspiration'

geely_car$aspiration<-as.factor(geely_car$aspiration)
levels(geely_car$aspiration)
levels(geely_car$aspiration)<-c(1,0)
geely_car$aspiration<-as.numeric(levels(geely_car$aspiration))[geely_car$aspiration]


# 6.5 Dummy Variable for 'doornumber'

geely_car$doornumber<-as.factor(geely_car$doornumber)
levels(geely_car$doornumber)
levels(geely_car$doornumber)<-c(1,0)
geely_car$doornumber<-as.numeric(levels(geely_car$doornumber))[geely_car$doornumber]


# 6.6 Dummy variable for 'carbody'

geely_car$carbody<-as.factor(geely_car$carbody)
levels(geely_car$carbody)

dummy_carbody<-data.frame((model.matrix(~carbody,data=geely_car)))
dummy_carbody<-dummy_carbody[,-1]

geely_car<-geely_car[,-5]
geely_car<-cbind(geely_car,dummy_carbody)
rm(dummy_carbody)


# 6.7 Dummy variable for 'drivewheel'

geely_car$drivewheel<-as.factor(geely_car$drivewheel)
levels(geely_car$drivewheel)

dummy_drivewheel<-data.frame((model.matrix(~drivewheel,data=geely_car)))
dummy_drivewheel<-dummy_drivewheel[,-1]

geely_car<-geely_car[,-5]
geely_car<-cbind(geely_car,dummy_drivewheel)
rm(dummy_drivewheel)


# 6.8 Dummy variable for 'enginelocation'

geely_car$enginelocation<-as.factor(geely_car$enginelocation)
levels(geely_car$enginelocation)
levels(geely_car$enginelocation)<-c(1,0)
geely_car$enginelocation<-as.numeric(levels(geely_car$enginelocation))[geely_car$enginelocation]


# 6.9 Dummy variable for 'enginetype'

geely_car$enginetype<-as.factor(geely_car$enginetype)
levels(geely_car$enginetype)

dummy_enginetype<-data.frame((model.matrix(~enginetype,data=geely_car)))
dummy_enginetype<-dummy_enginetype[,-1]

geely_car<-geely_car[,-11]
geely_car<-cbind(geely_car,dummy_enginetype)
rm(dummy_enginetype)


# 6.10 Dummy variable for 'cylindernumber'

geely_car$cylindernumber<-as.factor(geely_car$cylindernumber)
levels(geely_car$cylindernumber)

dummy_cylindernumber<-data.frame((model.matrix(~cylindernumber,data=geely_car)))
dummy_cylindernumber<-dummy_cylindernumber[,-1]

geely_car<-geely_car[,-11]
geely_car<-cbind(geely_car,dummy_cylindernumber)
rm(dummy_cylindernumber)


# 6.11 Dummy variable for 'fuelsystem'

geely_car$fuelsystem<-as.factor(geely_car$fuelsystem)
levels(geely_car$fuelsystem)

dummy_fuelsystem<-data.frame((model.matrix(~fuelsystem,data=geely_car)))
dummy_fuelsystem<-dummy_fuelsystem[,-1]

geely_car<-geely_car[,-11]
geely_car<-cbind(geely_car,dummy_fuelsystem)
rm(dummy_fuelsystem)




# ------------------------------------------------------------------------
# 7. Modelling ------------------------------------------------------------



# 7.1 Checking the structure of data set, discarding the variable
# 'carmodel' and converting whole data frame into a suitable 
# form for applying logistic regression

str(geely_car)
geely_car<-geely_car[,-1]
geely_car$curbweight<-as.numeric(geely_car$curbweight)


# 7.2 Setting seed and dividing into given data into train and 
# test data

set.seed(100)

trainindex<-sample(1:nrow(geely_car),0.7*nrow(geely_car))
geelytrain<-geely_car[trainindex,]
geelytest<-geely_car[-trainindex,]


# 7.3 Building the elementary model then using the mentioned logic for
# rejection of variables:
# First stepAIC  filter;
# Secondly based on VIF and p values both keeping
# check at R- squared value; 
# Then only remove using p- value keeping a check on 
# adjusted R- squared 
# Then predicting

model1<-lm(price~.,data = geelytrain)
summary(model1)


# 7.4 AIC filter

step<-stepAIC(model1,direction = "both")

step


#  Model 2

model2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
             carlength + carwidth + carheight + curbweight + boreratio + 
             compressionratio + peakrpm + highwaympg + carcompanybmw + 
             carcompanychevrolet + carcompanydodge + carcompanyhonda + 
             carcompanyisuzu + carcompanymazda + carcompanymercury + carcompanymitsubishi + 
             carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
             carcompanyrenault + carcompanysubaru + carcompanytoyota + 
             carcompanyvolkswagen + carcompanyvolvo + carbodyhardtop + 
             carbodyhatchback + carbodywagon + enginetypeohcv + enginetyperotor + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             cylindernumbertwelve + fuelsystem2bbl + fuelsystemmpfi + 
             carcompanyporsche, data = geelytrain)
vif(model2)
summary(model2)
# Removing wheelbase as significant VIF and high p- value


# Model 3

model3<-lm(formula = price ~ aspiration + enginelocation +             carlength + carwidth + carheight + curbweight + boreratio + 
             compressionratio + peakrpm + highwaympg + carcompanybmw + 
             carcompanychevrolet + carcompanydodge + carcompanyhonda + 
             carcompanyisuzu + carcompanymazda + carcompanymercury + carcompanymitsubishi + 
             carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
             carcompanyrenault + carcompanysubaru + carcompanytoyota + 
             carcompanyvolkswagen + carcompanyvolvo + carbodyhardtop + 
             carbodyhatchback + carbodywagon + enginetypeohcv + enginetyperotor + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             cylindernumbertwelve + fuelsystem2bbl + fuelsystemmpfi + 
             carcompanyporsche, data = geelytrain)
vif(model3)
summary(model3)
# Removing fuel sysytem mpfi


# Model 4 

model4<-lm(formula = price ~ aspiration + enginelocation +             carlength + carwidth + carheight + curbweight + boreratio + 
             compressionratio + peakrpm + highwaympg + carcompanybmw + 
             carcompanychevrolet + carcompanydodge + carcompanyhonda + 
             carcompanyisuzu + carcompanymazda + carcompanymercury + carcompanymitsubishi + 
             carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
             carcompanyrenault + carcompanysubaru + carcompanytoyota + 
             carcompanyvolkswagen + carcompanyvolvo + carbodyhardtop + 
             carbodyhatchback + carbodywagon + enginetypeohcv + enginetyperotor + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             cylindernumbertwelve + fuelsystem2bbl + 
             carcompanyporsche, data = geelytrain)
vif(model4)
summary(model4)
# Removing highwaympg


# Model 5

model5<-lm(formula = price ~ aspiration + enginelocation +             carlength + carwidth + carheight + curbweight + boreratio + 
             compressionratio + peakrpm + carcompanybmw + 
             carcompanychevrolet + carcompanydodge + carcompanyhonda + 
             carcompanyisuzu + carcompanymazda + carcompanymercury + carcompanymitsubishi + 
             carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
             carcompanyrenault + carcompanysubaru + carcompanytoyota + 
             carcompanyvolkswagen + carcompanyvolvo + carbodyhardtop + 
             carbodyhatchback + carbodywagon + enginetypeohcv + enginetyperotor + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             cylindernumbertwelve + fuelsystem2bbl + 
             carcompanyporsche, data = geelytrain)
vif(model5)
summary(model5)
# Removing the variable carcompanyporsche


# Model 6 

model6<-lm(formula = price ~ aspiration + enginelocation + carlength + 
            carwidth + carheight + curbweight + boreratio + compressionratio + 
            peakrpm + carcompanybmw + carcompanychevrolet + carcompanydodge + 
            carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
            carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
            carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
            carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
            carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
            enginetyperotor + cylindernumberfive + cylindernumberfour + 
            cylindernumbersix + cylindernumbertwelve + fuelsystem2bbl
            , data = geelytrain) 
vif(model6)
summary(model6)
# Removing the insignificant covariate carcompanynissan


# Model 7

model7<-lm(formula = price ~ aspiration + enginelocation + carlength + 
             carwidth + carheight + curbweight + boreratio + compressionratio + 
             peakrpm + carcompanybmw + carcompanychevrolet + carcompanydodge + 
             carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
             carcompanymitsubishi + carcompanypeugeot + 
             carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
             carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
             carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
             enginetyperotor + cylindernumberfive + cylindernumberfour + 
             cylindernumbersix + cylindernumbertwelve + fuelsystem2bbl, 
           data = geelytrain)
vif(model7)
summary(model7)
# Removing the  predictor variable aspiration


# Model 8

model8<-lm(formula = price ~ enginelocation + carlength + 
             carwidth + carheight + curbweight + boreratio + compressionratio + 
             peakrpm + carcompanybmw + carcompanychevrolet + carcompanydodge + 
             carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
             carcompanymitsubishi + carcompanypeugeot + 
             carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
             carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
             carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
             enginetyperotor + cylindernumberfive + cylindernumberfour + 
             cylindernumbersix + cylindernumbertwelve + fuelsystem2bbl, 
           data = geelytrain)
vif(model8)
summary(model8)
# Removing the insignificant predictor variable fuelsysytem2bbl 


# Model 9

model9<-lm(formula = price ~ enginelocation + carlength + 
             carwidth + carheight + curbweight + boreratio + compressionratio + 
             peakrpm + carcompanybmw + carcompanychevrolet + carcompanydodge + 
             carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
             carcompanymitsubishi + carcompanypeugeot + 
             carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
             carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
             carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
             enginetyperotor + cylindernumberfive + cylindernumberfour + 
             cylindernumbersix + cylindernumbertwelve , 
           data = geelytrain)
vif(model9)
summary(model9)
# Removing the insignificant predictor variable enginelocation


# Model 10

model10<-lm(formula = price ~ carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              peakrpm + carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
              carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + cylindernumbertwelve , 
            data = geelytrain)
vif(model10)
summary(model10)

model10final<-lm(formula = price ~ enginelocation + carlength + 
                           carwidth + carheight + curbweight + boreratio + compressionratio + 
                           carcompanybmw + carcompanychevrolet + carcompanydodge + 
                           carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
                           carcompanymitsubishi + carcompanypeugeot + 
                           carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
                           carcompanytoyota + carcompanyvolkswagen + carcompanyvolvo + 
                           carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
                           enginetyperotor + cylindernumberfive + cylindernumberfour + 
                           cylindernumbersix + cylindernumbertwelve , 
                         data = geelytrain)
vif(model10final)
summary(model10final)
# As model10's Adjusted R squared dropped significantly on removing 
# enginelocation hence we removed peakrpm and there was insignificant
# change in the Adjusted R squared.
# Next Removing the insignificant variable carcompanyvolvo


# Model 11

model11<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen+ 
              carbodyhardtop + carbodyhatchback + carbodywagon + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + cylindernumbertwelve , 
            data = geelytrain)
vif(model11)
summary(model11)
# Removing the insignificant predictor variable carbodyhardtop


# Model 12 

model12<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + carbodyhatchback + carbodywagon + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + cylindernumbertwelve , 
            data = geelytrain)
vif(model12)
summary(model12)
# Removing the insignificant covariate carbodyhatchback


# Model 13

model13<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + carbodywagon + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + cylindernumbertwelve , 
            data = geelytrain)
vif(model13)
summary(model13)
# Next removing the insignificant predictor carbodywagon


# Model 14 

model14<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix + cylindernumbertwelve , 
            data = geelytrain)
vif(model14)
summary(model14)
# Next removing insignificant predictor cylindernumbertwelve


# Model 15 

model15<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + compressionratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
vif(model15)
summary(model15)
# Removing the insignificant covariate compressionratio


# Model 16 

model16<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanyisuzu + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
vif(model16)
summary(model16)
# Removing the insignificant covariate carcompanyisuzu


# Model 17

model17<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
vif(model17)
summary(model17)
# Removing the insignificant covariate carcompanymazda

# Model 18 

model18<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + carcompanysubaru + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
vif(model18)
summary(model18)
# Removing the next insignificant covariate carcompanysubaru


# Model 19

model19<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw + carcompanychevrolet + carcompanydodge + 
              carcompanyhonda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + carcompanyrenault + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
vif(model19)
summary(model19)
# Removing the insignificant covariates  carcompanychevrolet,
# carcompanymercury,carcompanyrenault


# Model 20

model20<-lm(formula = price ~ enginelocation + carlength + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw  + carcompanydodge + 
              carcompanyhonda + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + 
              carcompanytoyota + carcompanyvolkswagen + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
summary(model20)
# Removing carcompanyvolkwagen, carcompanydodge in subsequent model


# Model 21

model21<-lm(formula = price ~ enginelocation + carlength + 
                       carwidth + carheight + curbweight + boreratio + 
                       carcompanybmw + 
                       carcompanyhonda + 
                       carcompanymitsubishi + carcompanypeugeot + 
                       carcompanyplymouth + 
                       carcompanytoyota + enginetypeohcv + 
                       enginetyperotor + cylindernumberfive + cylindernumberfour + 
                       cylindernumbersix , 
                     data = geelytrain)
summary(model21)
vif(model21)
# Removing the insignificant variable carlength


# Model 22

model22<-lm(formula = price ~ enginelocation + 
              carwidth + carheight + curbweight + boreratio + 
              carcompanybmw + 
              carcompanyhonda + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + 
              carcompanytoyota + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
summary(model22)
# Removing carcompanyhonda


# Model 23 

model23<-lm(formula = price ~ enginelocation + 
             carwidth + carheight + curbweight + boreratio + 
             carcompanybmw + 
             carcompanymitsubishi + carcompanypeugeot + 
             carcompanyplymouth + 
             carcompanytoyota + enginetypeohcv + 
             enginetyperotor + cylindernumberfive + cylindernumberfour + 
             cylindernumbersix , 
           data = geelytrain)
summary(model23)
vif(model23)
# Removing carwidth and seeing the drop in Adjusted R squared value


# Model 24

model24<-lm(formula = price ~ enginelocation + 
              carheight + curbweight + boreratio + 
              carcompanybmw + 
              carcompanymitsubishi + carcompanypeugeot + 
              carcompanyplymouth + 
              carcompanytoyota + enginetypeohcv + 
              enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix , 
            data = geelytrain)
summary(model24)
# Now the Adjusted R square value is dropping sharply compared to 
# initial subsequent models so this 'model24' can be taken as 
# a good regression model for the business problem




# ------------------------------------------------------------------------
# 8. Evaluation and conclusion --------------------------------------------


# Predicting the results on test data set

predict_geely_car<-predict(model24,geelytest[,-1])
geelytest$predicted_price<-predict_geely_car

# Checking the r square between the actual and the predicted value

r<-cor(geelytest$predicted_price,geelytest$price)
rsquared<-r^2
rsquared


## CONCLUSION: Hence the model "model 24" describes all the factors
## which pose as a competetive variable for geely car manufacturing
## company when they enter the US car market as per the historical
## data. The model also explains the variance upto 87 % which 
## is a modest prediction indication.




# ------------------------------------------------------------------------
# END ---------------------------------------------------------------------


