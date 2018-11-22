#Key Observations:
#1.The total investment for Venture,Seed,Angel,Private_Equity FT differs after
#merging the master_frame with the 8 main sectors.This observation is seen due to the removal of NA's and Blanks
#from the primary_sector.


#Checkpoint 1: Data Cleaning 1
#Loading the companies and rounds data
#into two data frames and naming them companies and rounds2 respectively.

companies<-read.delim("companies.txt",stringsAsFactors= FALSE, header = TRUE)
View(companies)

rounds2<-read.csv("rounds2.csv",stringsAsFactors= FALSE,header = TRUE)
View(rounds2)

# TABLE 1.1 Understanding the Data Set

library(dplyr)

#1.How many unique companies are present in rounds2?

unique_companies_rounds2<-distinct(rounds2,tolower(company_permalink))
nrow(unique_companies_rounds2)
 

#2.How many unique companies are present in the companies file?

unique_companies_companies<-distinct(companies,tolower(permalink))
nrow(unique_companies_companies)


#3.In the companies data frame, which column can be used as the 
#  unique key for each company? Write the name of the column.

#permalink

names(companies)[1]


#4.Are there any companies in the rounds2 file which
#  are not present in companies? 

rounds2$company_permalink<-trimws(rounds2$company_permalink)
companies$permalink<-trimws(companies$permalink)

colnames(companies)[1]<-"company_permalink"

companies$company_permalink<-tolower(companies$company_permalink)
rounds2$company_permalink<-tolower(rounds2$company_permalink)

length(rounds2$company_permalink[!(rounds2$company_permalink %in% companies$company_permalink)])
#N

#5.Merge the two data frames so that all variables (columns) 
#  in the companies frame are added to the rounds2 data frame. 
#  Name the merged frame master_frame. How many observations are 
#  present in master_frame?

master_frame<-merge(rounds2,companies,by = "company_permalink")
nrow(master_frame)
View(master_frame)

#............................................................................................

#Checkpoint 2: Funding Type Analysis
# TABLE 2.1  Average Values of Investments for Each of these Funding Types		

library(dplyr)

dffunding_round_type<-group_by(master_frame,funding_round_type)
dffunding_round_type<-summarise(dffunding_round_type,average_funding=mean(raised_amount_usd,na.rm = TRUE))
View(dffunding_round_type)


#1. Average funding amount of venture type
#   11748949

funding_venture<-filter(dffunding_round_type,funding_round_type=="venture")
funding_venture$average_funding[1]

#2.Average funding amount of angel type
#   958694

funding_angel<-filter(dffunding_round_type,funding_round_type=="angel")
round(funding_angel$average_funding[1],0)

#3.Average funding amount of seed type
#   719818

funding_seed<-filter(dffunding_round_type,funding_round_type=="seed")
funding_seed$average_funding[1]


#4.Average funding amount of private equity type
#   73308593

funding_private_equity<-filter(dffunding_round_type,funding_round_type=="private_equity")
funding_private_equity$average_funding[1]


#5.Considering that Spark Funds wants to invest between 
#  5 to 15 million USD per investment round, which investment 
#  type is the most suitable for them?

suitable_investment_type<-arrange(dffunding_round_type,desc(average_funding))
suitable_investment_type<-filter(suitable_investment_type,average_funding>=5000000 & average_funding<=15000000)
FT<-suitable_investment_type$funding_round_type[1]
FT

#  Therefore VENTURE Funding is most suitable  


#..............................................................................................
#Checkpoint 3: Country Analysis

library(dplyr)
master_frame_venture<-filter(master_frame,funding_round_type=="venture")
View(master_frame_venture)

#1.Spark Funds wants to see the top nine countries which have received 
#  the highest total funding 
#  (across ALL sectors for the chosen investment type)

top_nine_countries_g<-group_by(master_frame_venture,country_code)
top_nine_countries_s<-summarise(top_nine_countries_g,total_funding=sum(raised_amount_usd,na.rm = TRUE))

top_nine_countries_a<-arrange(top_nine_countries_s,desc(total_funding))
top_nine_countries_a<-top_nine_countries_a[-which(top_nine_countries_a$country_code==""),]

top_nine_countries<-top_nine_countries_a$country_code[1:9]
top_nine_countries

#2.For the chosen investment type, make a data frame named top9 
#  with the top nine countries 
#  (based on the total investment amount each country has received)

top9<-filter(master_frame_venture,country_code==top_nine_countries)
View(top9)


#Table 3.1: Analysing the Top 3 English-Speaking Countries

library(countrycode)
top_nine_countries<- countrycode(top_nine_countries, origin="iso3c", destination="country.name")
top_nine_countries 

#After comparing manually with the pdf file provided for list of english speaking countries

#1.Top English speaking country

top_english_speaking_country<-top_nine_countries[1]
top_english_speaking_country

#2.Second English speaking country

second_english_speaking_country<-top_nine_countries[3]
second_english_speaking_country

#3.Third English speaking country

third_english_speaking_country<-top_nine_countries[4]
third_english_speaking_country


#........................................................................................
#Checkpoint 4: Sector Analysis 1


library(stringr)

#1.Extract the primary sector of each category list from the 
#  category_list column

master_frame<-mutate(master_frame,primary_sector=word(master_frame$category_list,1,sep=fixed("|")))
master_frame$primary_sector
View(master_frame)


#2.Use the mapping file 'mapping.csv' to map each primary sector 
#  to one of the eight main sectors 
#  (Note that 'Others' is also considered one of the main sectors)

library(tidyr)
mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE, header= TRUE)
mapping$category_list<-str_replace_all(mapping$category_list, '0', 'na')
mapping$category_list<-str_replace_all(mapping$category_list, 'Enterprise 2.na', 'Enterprise 2.0')
View(mapping)

colnames(mapping)[1]<-"primary_sector"
mapping$primary_sector<-tolower(mapping$primary_sector)
master_frame$primary_sector<-tolower(master_frame$primary_sector)


master_frame_merged<-merge(master_frame,mapping,by = "primary_sector",all.x = TRUE)

master_frame_gathered<-gather(master_frame_merged,main_sector,val,Automotive...Sports:Social..Finance..Analytics..Advertising)
master_frame_gathered<-master_frame_gathered[!(master_frame_gathered$val==0),]

master_frame_gathered<-master_frame_gathered[,-18]

master_frame_gathered<-filter(master_frame_gathered,main_sector!="Blanks")
write.csv(master_frame_gathered,file="master_frame_gath.csv") 
View(master_frame_gathered)

#master_frame_gathered is the expected data frame


#.....................................................................................................................

#Checkpoint 5: Sector Analysis 2
#1.Three data frames D1, D2 and D3 

#D1
D1<-filter(master_frame_gathered,country_code=="USA",raised_amount_usd>=5000000,raised_amount_usd<=15000000,funding_round_type==FT)


D1total_amount_main_sector<-aggregate(D1$raised_amount_usd,by=list(D1$main_sector),FUN=sum)
D1total_amount_main_sector<-setNames(D1total_amount_main_sector,c("main_sector","total_investment_main_sector"))
D1total_amount_main_sector

D1investment_count_main_sector<-group_by(D1,main_sector)
D1investment_count_main_sector<-summarise(D1investment_count_main_sector,investment_count_main_sector=n())
D1investment_count_main_sector

D1<-merge(D1,D1total_amount_main_sector,by="main_sector")
D1<-merge(D1,D1investment_count_main_sector,by="main_sector")
View(D1)

#D2

D2<-filter(master_frame_gathered,country_code=="GBR",raised_amount_usd>=5000000,raised_amount_usd<=15000000,funding_round_type==FT)

D2total_amount_main_sector<-aggregate(D2$raised_amount_usd,by=list(D2$main_sector),FUN=sum)
D2total_amount_main_sector<-setNames(D2total_amount_main_sector,c("main_sector","total_investment_main_sector"))
D2total_amount_main_sector

D2investment_count_main_sector<-group_by(D2,main_sector)
D2investment_count_main_sector<-summarise(D2investment_count_main_sector,investment_count_main_sector=n())
D2investment_count_main_sector


D2<-merge(D2,D2total_amount_main_sector,by="main_sector")


D2<-merge(D2,D2investment_count_main_sector,by="main_sector")
View(D2)


#D3

D3<-filter(master_frame_gathered,country_code=="IND",raised_amount_usd>=5000000,raised_amount_usd<=15000000,funding_round_type==FT)

D3total_amount_main_sector<-aggregate(D3$raised_amount_usd,by=list(D3$main_sector),FUN=sum)
D3total_amount_main_sector<-setNames(D3total_amount_main_sector,c("main_sector","total_investment_main_sector"))
D3total_amount_main_sector

D3investment_count_main_sector<-group_by(D3,main_sector)
D3investment_count_main_sector<-summarise(D3investment_count_main_sector,investment_count_main_sector=n())
D3investment_count_main_sector


D3<-merge(D3,D3total_amount_main_sector,by="main_sector")
D3<-merge(D3,D3investment_count_main_sector,by="main_sector")
View(D3)


#Table 5.1 : Sector-wise Investment Analysis

#1.Total number of investments (count)

#for C1

C1<-group_by(D1,main_sector)
C1<-summarise(C1,investment_count_main_sector=n())
C1count_total_investment<-sum(C1$investment_count_main_sector)
C1count_total_investment


#for C2

C2<-group_by(D2,main_sector)
C2<-summarise(C2,investment_count_main_sector=n())
C2count_total_investment<-sum(C2$investment_count_main_sector)
C2count_total_investment


#for C3

C3<-group_by(D3,main_sector)
C3<-summarise(C3,investment_count_main_sector=n())
C3count_total_investment<-sum(C3$investment_count_main_sector)
C3count_total_investment


#2.Total amount of investment (USD)

#for C1

C1total_investment<-sum(D1$raised_amount_usd)
C1total_investment

#for C2

C2total_investment<-sum(D2$raised_amount_usd)
C2total_investment

#for C3

C3total_investment<-sum(D3$raised_amount_usd)
C3total_investment


#3.Top sector (based on count of investments)

#for C1

C1top<-filter(C1,investment_count_main_sector==(sort(C1$investment_count_main_sector,decreasing = TRUE)[1]))
C1top_sector<-C1top$main_sector
C1top_sector


#for C2

C2top<-filter(C2,investment_count_main_sector==(sort(C2$investment_count_main_sector,decreasing = TRUE)[1]))
C2top_sector<-C2top$main_sector
C2top_sector


#for C3

C3top<-filter(C3,investment_count_main_sector==(sort(C3$investment_count_main_sector,decreasing = TRUE)[1]))
C3top_sector<-C3top$main_sector
C3top_sector


#4.Second-best sector (based on count of investments)

#for C1

C1second_best<-filter(C1,investment_count_main_sector==(sort(C1$investment_count_main_sector,decreasing = TRUE)[2]))
C1second_best_sector<-C1second_best$main_sector
C1second_best_sector


#for C2

C2second_best<-filter(C2,investment_count_main_sector==(sort(C2$investment_count_main_sector,decreasing = TRUE)[2]))
C2second_best_sector<-C2second_best$main_sector
C2second_best_sector

#for C3

C3second_best<-filter(C3,investment_count_main_sector==(sort(C3$investment_count_main_sector,decreasing = TRUE)[2]))
C3second_best_sector<-C3second_best$main_sector
C3second_best_sector

#5.Third-best sector (based on count of investments)

#for C1

C1third_best<-filter(C1,investment_count_main_sector==(sort(C1$investment_count_main_sector,decreasing = TRUE)[3]))
C1third_best_sector<-C1third_best$main_sector
C1third_best_sector

#for C2

C2third_best<-filter(C2,investment_count_main_sector==(sort(C2$investment_count_main_sector,decreasing = TRUE)[3]))
C2third_best_sector<-C2third_best$main_sector
C2third_best_sector

#for C3

C3third_best<-filter(C3,investment_count_main_sector==(sort(C3$investment_count_main_sector,decreasing = TRUE)[3]))
C3third_best_sector<-C3third_best$main_sector
C3third_best_sector



#6.Number of investments in the top sector (refer to point 3)

#for C1

C1_top_investment_count<-sort(C1$investment_count_main_sector,decreasing = TRUE)[1]
C1_top_investment_count


#for C2

C2_top_investment_count<-sort(C2$investment_count_main_sector,decreasing = TRUE)[1]
C2_top_investment_count

#for C3

C3_top_investment_count<-sort(C3$investment_count_main_sector,decreasing = TRUE)[1]
C3_top_investment_count



#7.Number of investments in the second-best sector (refer to point 4)

#for C1
C1_secondbest_investment_count<-sort(C1$investment_count_main_sector,decreasing = TRUE)[2]
C1_secondbest_investment_count

#for C2
C2_secondbest_investment_count<-sort(C2$investment_count_main_sector,decreasing = TRUE)[2]
C2_secondbest_investment_count

#for C3
C3_secondbest_investment_count<-sort(C3$investment_count_main_sector,decreasing = TRUE)[2]
C3_secondbest_investment_count


#8.Number of investments in the third-best sector (refer to point 5)

#for C1
C1_thirdbest_investment_count<-sort(C1$investment_count_main_sector,decreasing = TRUE)[3]
C1_thirdbest_investment_count

#for C2
C2_thirdbest_investment_count<-sort(C2$investment_count_main_sector,decreasing = TRUE)[3]
C2_thirdbest_investment_count

#for C3

C3_thirdbest_investment_count<-sort(C3$investment_count_main_sector,decreasing = TRUE)[3]
C3_thirdbest_investment_count


#9.For the top sector count-wise (point 3), 
#  which company received the highest investment?
  
#for C1


C1top_sector_highest_investment<-filter(D1,main_sector==C1top_sector)
C1top_sector_highest_investment<-group_by(C1top_sector_highest_investment,name)
C1top_sector_highest_investment<-summarise(C1top_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C1top_sector_highest_investment<-filter(C1top_sector_highest_investment,total_investment_company==max(total_investment_company))
C1top_sector_highest_investment_company<-C1top_sector_highest_investment$name
C1top_sector_highest_investment_company


#for C2

C2top_sector_highest_investment<-filter(D2,main_sector==C2top_sector)
C2top_sector_highest_investment<-group_by(C2top_sector_highest_investment,name)
C2top_sector_highest_investment<-summarise(C2top_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C2top_sector_highest_investment<-filter(C2top_sector_highest_investment,total_investment_company==max(total_investment_company))
C2top_sector_highest_investment_company<-C2top_sector_highest_investment$name
C2top_sector_highest_investment_company



#for C3

C3top_sector_highest_investment<-filter(D3,main_sector==C3top_sector)
C3top_sector_highest_investment<-group_by(C3top_sector_highest_investment,name)
C3top_sector_highest_investment<-summarise(C3top_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C3top_sector_highest_investment<-filter(C3top_sector_highest_investment,total_investment_company==max(total_investment_company))
C3top_sector_highest_investment_company<-C3top_sector_highest_investment$name
C3top_sector_highest_investment_company




#10.For the second-best sector count-wise (point 4),
#   which company received the highest investment?
#for C1

C1second_sector_highest_investment<-filter(D1,main_sector==C1second_best_sector)
C1second_sector_highest_investment<-group_by(C1second_sector_highest_investment,name)
C1second_sector_highest_investment<-summarise(C1second_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C1second_sector_highest_investment<-filter(C1second_sector_highest_investment,total_investment_company==max(total_investment_company))
C1second_sector_highest_investment_company<-C1second_sector_highest_investment$name
C1second_sector_highest_investment_company



#for C2

C2second_sector_highest_investment<-filter(D2,main_sector==C2second_best_sector)
C2second_sector_highest_investment<-group_by(C2second_sector_highest_investment,name)
C2second_sector_highest_investment<-summarise(C2second_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C2second_sector_highest_investment<-filter(C2second_sector_highest_investment,total_investment_company==max(total_investment_company))
C2second_sector_highest_investment_company<-C2second_sector_highest_investment$name
C2second_sector_highest_investment_company


#for C3

C3second_sector_highest_investment<-filter(D3,main_sector==C3second_best_sector)
C3second_sector_highest_investment<-group_by(C3second_sector_highest_investment,name)
C3second_sector_highest_investment<-summarise(C3second_sector_highest_investment,total_investment_company=sum(raised_amount_usd))
C3second_sector_highest_investment<-filter(C3second_sector_highest_investment,total_investment_company==max(total_investment_company))
C3second_sector_highest_investment_company<-C3second_sector_highest_investment$name
C3second_sector_highest_investment_company


