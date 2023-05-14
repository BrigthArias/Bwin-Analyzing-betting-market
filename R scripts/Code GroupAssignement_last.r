############################## PACKAGES NEEDED IN ALL THE CODING ##############################

if(!require("haven")) install.packages(c("haven", "sas7bdat")); library("haven")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("magrittr")) install.packages("magrittr"); library("magrittr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("htmlwidgets")) install.packages("htmlwidgets"); library("htmlwidgets")

############################## READING THE DATA SETS ##############################

#install.packages(c("haven", "sas7bdat"))

#We used this library in order to read the data in R from SAS

library(haven)

#Code for reading the RawDataI

DataI_Demo <- read_sas('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\RawDataIDemographics.sas7bdat')
summary(DataI_Demo)

#Code for reading the RawDataII

DataII_Uda <- read_sas('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\RawDataIIUserDailyAggregation.sas7bdat')
summary(DataII_Uda)

#Code for reading the RawDataIII

DataIII_Pcc <- read_sas('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\RawDataIIIPokerChipConversions.sas7bdat')
summary(DataIII_Pcc)

############################## PROCESS OF THE DATA SET DataI_Demo ##############################
#See the class of each variable of the data frame DataI_Demo
str(DataI_Demo)

#CONVERT EACH VARIABLE NEEDED

#UserID <- num - chr*
#RegDate <- chr - date
# All "first" columns <- chr - date

#Converting the UserID from numeric into character
DataI_Demo$UserID <- as.character(DataI_Demo$UserID)

sapply(DataI_Demo, class) #Getting the classes of the columns, indeed the column was converted into chr

#See the na in gender (CHANGE)
sum(is.na(DataI_Demo$Gender))
# Replace the missing value of gender for 1 (CHANGE)
DataI_Demo[is.na(DataI_Demo$Gender),"Gender"] <- 1 

# Rename the Gender column (CHANGE)
DataI_Demo <- DataI_Demo %>% mutate(Gender = ifelse(Gender == 0,"Female","Male")) 


#Converting the columns 4,5,6,7,8,9 and 10 from character into date

library(lubridate)

DataI_Demo$RegDate <- ymd(DataI_Demo$RegDate)
DataI_Demo$FirstPay <- ymd(DataI_Demo$FirstPay)
DataI_Demo$FirstAct <- ymd(DataI_Demo$FirstAct)
DataI_Demo$FirstSp <- ymd(DataI_Demo$FirstSp)
DataI_Demo$FirstCa <- ymd(DataI_Demo$FirstCa)
DataI_Demo$FirstGa <- ymd(DataI_Demo$FirstGa)
DataI_Demo$FirstPo <- ymd(DataI_Demo$FirstGa)

#The numbers of failed to parse are the NA cells of each column
sum(is.na(DataI_Demo$FirstSp)) #Example of DataI_Demo$FirstSp

sapply(DataI_Demo, class) #Getting the classes of the columns, indeed the columns were converted into Date

############################## PROCESS OF THE DATA SET DataII_Uda ##############################
#Inspect the DAtaII:User daily aggregation
str(DataII_Uda)

# Change UserID type
DataII_Uda$UserID <- as.character(DataII_Uda$UserID)

# Change the date from character to date
DataII_Uda$Date <- ymd(DataII_Uda$Date)

# Round Stakes
DataII_Uda$Stakes <- round(DataII_Uda$Stakes, 2)

# Round Winnings
DataII_Uda$Winnings <- round(DataII_Uda$Winnings, 2)

sapply(DataII_Uda, class)

############################## PROCESS OF THE DATA SET DataIII_Pcc ##############################
#Inspect dataIII_Pcc: Transaction table
str(DataIII_Pcc)
head(DataIII_Pcc)

# Change UserID type
DataIII_Pcc$UserID <- as.character(DataIII_Pcc$UserID)

library(magrittr)

# Change the datetime from character to date format for
DataIII_Pcc <- DataIII_Pcc %>% 
  mutate(
    TransDatePoker = as.Date(TransDateTime),
    hour_TimePoker = hour(TransDateTime),
    minute_TimePoker = minute(TransDateTime),
    second_TimePoker = second(TransDateTime)
  ) %>% 
  mutate(
    TransHourPocker = paste(hour_TimePoker, minute_TimePoker, second_TimePoker, sep = ":")
  )

# Round TransAmount

DataIII_Pcc$TransAmount <- round(DataIII_Pcc$TransAmount, 2)

#Add a column 'ProductID' with his own code

DataIII_Pcc$ProductID = 3

sapply(DataIII_Pcc, class)

############################## MERGING ALL THE DATA SETS ##############################

#For merging all the data, we use the library dplyr

library(dplyr)

#Joinning the three data sets (we used the left_join because we are analyzing 
  #the demographic information with their corresponding transaction information)

#Joinning the data II and data III
DataII_III <- DataII_Uda %>% full_join(DataIII_Pcc, by= c('ProductID', 'UserID'))

#Joinning all the data
DataMart <- DataI_Demo %>% left_join(DataII_III, by='UserID')

summary(DataMart)
unique(DataMart$ProductID)

#Here we can see that there are NA values in  FirstAct, so we are going to take them off to clients who have missing values for first active date
sum(is.na(DataMart$FirstAct))

#So we use the library tidyr to eliminate the rows with NA in FirstAct
library(tidyr)

DataMart <- drop_na(DataMart, FirstAct)

#Exclude records in the raw dataset UserDailyAggregation that took place before the 
 #first pay-in date (i.e., variable FirstPay in raw dataset Demographics) in the preparation of 
 #the data mart.

DataMart <- subset(DataMart, FirstPay < Date| is.na(Date))

sum(is.na(DataMart$FirstPay))
sum(is.na(DataMart$Date))

head(DataMart$Date)

#To verify that all the products ID are in the DataMart
unique(DataMart$ProductID)

sapply(DataMart, class)

unique(DataMart$ProductID)

#install.packages('readxl')

#Using this lybrary for reading the excel file
library(readxl)

#Reading the Appendix 1
AppProductID=read_excel('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\Appendices Group Assignment.xlsx', 'Appendix 1')
AppProductID

#Reading the Appendix 2
AppCountry=read_excel('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\Appendices Group Assignment.xlsx', 'Appendix 2')
AppCountry

#Reading the Appendix 3
AppLanguage=read_excel('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\Appendices Group Assignment.xlsx', 'Appendix 3')
AppLanguage

#Reading the Appendix 4
AppApplication=read_excel('C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\Appendices Group Assignment.xlsx', 'Appendix 4')
AppApplication

#Joinning the Appendix 1 to the Datamart: The product description with the productID and rename product description
DataMart <- DataMart %>% left_join(AppProductID, by='ProductID')%>% rename(Product_Description = `Product Description`)

#Joinning the Appendix 2 to the Datamart: The Country with the Country Name
DataMart <- DataMart %>% left_join(AppCountry, by='Country')%>% rename(Country_Name = `Country Name`)

#Joinning the Appendix 3 to the Datamart: The Language with the Language Description
DataMart <- DataMart %>% left_join(AppLanguage, by='Language')%>% rename(Language_Description = `Language Description`)

#Joinning the Appendix 4 to the Datamart: The ApplicationID with the Application Description
DataMart <- DataMart %>% left_join(AppApplication, by='ApplicationID')%>% rename(Application_Description = `Application Description`)
glimpse(DataMart)

############################## CREATION OF NEW VARIABLES FOR DATA MART MARKETING ##############################

#1.First date active of each users
FirstActiveDate <- data.frame(UserID = DataMart$UserID, FirstAct =  DataMart$FirstAct, TransDatePoker = DataMart$TransDatePoker)
head(FirstActiveDate)

FirstActiveDate <- FirstActiveDate %>% 
  group_by(UserID) %>%
  summarise(First_Active_Date = min(c(min(FirstAct),min(TransDatePoker)), na.rm = TRUE))#Take the minimun of the First Active date as the earliest
head(FirstActiveDate)

#2.Last date active of each users
LastActiveDate <- data.frame(UserID = DataMart$UserID, Date =  DataMart$Date, TransDatePoker = DataMart$TransDatePoker)
head(LastActiveDate)

LastActiveDate <- LastActiveDate %>% 
  group_by(UserID) %>%
  summarise(Last_Active_Date = c(max(max(Date, na.rm = TRUE), max(TransDatePoker, na.rm = TRUE))))#Take the maximum of the date as the latest

##RFM for betting products##
#Create recency, frecuency, and monetary of the betting product(all products, except the pokerBoosMedia, product 3)
#Create data frame
rfm_df_betting <- data.frame(UserID = DataII_Uda$UserID, Date =  DataII_Uda$Date, Stakes = DataII_Uda$Stakes)

#Define day of analysis which is the last day of the period analysis
analysis_date <- as_date("2005-09-30")

#Define the rfm_betting
rfm_betting <- rfm_df_betting %>% 
  group_by(UserID) %>% 
  summarise(Recency = as.numeric(analysis_date- max(Date)), Frequency = n(), Monetary = sum(Stakes))
head(rfm_betting)
glimpse(rfm_betting)

#Number of rows of the customer based in the DataII_Uda
nrow(rfm_betting)
#See the summary
summary(rfm_betting)

#Giving values of recency to each customer (VERIFICAR)
rfm_betting$R_score <- 0 
rfm_betting$R_score[rfm_betting$Recency >= 191.00] <- 1 #values grater than 3th quantile
rfm_betting$R_score[rfm_betting$Recency >= 74.00 & rfm_betting$Recency <191.00] <- 2 #values grater than 2th quantile
rfm_betting$R_score[rfm_betting$Recency >= 5.00 & rfm_betting$Recency <74.00] <- 3 #values grater than 1th quantile
rfm_betting$R_score[rfm_betting$Recency < 5.00] <- 4 #values lower than 1th quantile

#Giving values of frecuency to each customer
rfm_betting$F_score<- 0
rfm_betting$F_score[rfm_betting$Frequency >=52.00] <- 4
rfm_betting$F_score[rfm_betting$Frequency <52.00 & rfm_betting$Frequency >= 22.00] <- 3
rfm_betting$F_score[rfm_betting$Frequency <22.00 & rfm_betting$Frequency >= 8.00] <- 2
rfm_betting$F_score[rfm_betting$Frequency <8.00] <- 1

#Giving values of monetary to each customer
rfm_betting$M_score <- 0
rfm_betting$M_score[rfm_betting$Monetary >= 819.2] <- 4
rfm_betting$M_score[rfm_betting$Monetary < 819.2 & rfm_betting$Monetary >= 233.8] <- 3
rfm_betting$M_score[rfm_betting$Monetary >= 77.7 & rfm_betting$Monetary < 233.8] <- 2
rfm_betting$M_score[rfm_betting$Monetary <77.7] <- 1

#Create the rfm score
rfm_betting <- rfm_betting %>% mutate(RFM_score_betting = 100 *R_score +10 * F_score + M_score)

#segmenting to clients into 5 groups
rfm_betting<-rfm_betting%>%mutate(Loyalty_segmentation_bettings= case_when(
  between(RFM_score_betting,0,178)~"Can’t Lose Them",
  between(RFM_score_betting,179,244)~"At Risk Customers",
  between(RFM_score_betting,245,311)~"New Customers",
  between(RFM_score_betting,311,377)~"Potential Loyalists",
  TRUE~"Champions"))

summary(rfm_betting)
#See results
head(rfm_betting,20)

###Graphic####

table(rfm_betting$Loyalty_segmentation_bettings)
ggplot(rfm_betting) + geom_bar(aes(x = Loyalty_segmentation_bettings, fill = Loyalty_segmentation_bettings))+theme(axis.text.x=element_text(angle=90,hjust=1)) +labs(title = "Barplot for Segments of customers")


#Select columns in order to join to dataMartMarketing
rfm_betting <- rfm_betting%>%
  select(UserID, RFM_score_betting, Loyalty_segmentation_bettings)

##RFM for poker chip product##
#Create recency, frecuency, and monetary of the poker chip product, product 3.
#Create data frame
rfm_df_pokerChip <- data.frame(UserID = DataIII_Pcc$UserID, DatePoker=  DataIII_Pcc$TransDatePoker, TransType = DataIII_Pcc$TransType, TransAmount = DataIII_Pcc$TransAmount)

#Define the rfm_betting
rfm_df_pokerChip <- rfm_df_pokerChip%>%
  group_by(UserID) %>%
  summarise(Monetary = sum(TransAmount[TransType==124]),
            Frequency = n(),
            Recency = as.numeric(as_date("2005-09-30")- max(DatePoker))
            )

#Number of rows of the customer based in the DataII_Uda
nrow(rfm_df_pokerChip)
#See the summary
summary(rfm_df_pokerChip)

#Giving values of recency to each customer
rfm_df_pokerChip$R_score <- 0 
rfm_df_pokerChip$R_score[rfm_df_pokerChip$Recency >= 202] <- 1 #values grater than 3th quantile
rfm_df_pokerChip$R_score[rfm_df_pokerChip$Recency >= 91 & rfm_df_pokerChip$Recency <202] <- 2 #values grater than 2th quantile
rfm_df_pokerChip$R_score[rfm_df_pokerChip$Recency >= 9 & rfm_df_pokerChip$Recency <91] <- 3 #values grater than 1th quantile
rfm_df_pokerChip$R_score[rfm_df_pokerChip$Recency < 9] <- 4 #values lower than 1th quantile

#Giving values of frecuency to each customer
rfm_df_pokerChip$F_score<- 0
rfm_df_pokerChip$F_score[rfm_df_pokerChip$Frequency >=127.5] <- 4
rfm_df_pokerChip$F_score[rfm_df_pokerChip$Frequency <127.5 & rfm_df_pokerChip$Frequency >= 29.0] <- 3
rfm_df_pokerChip$F_score[rfm_df_pokerChip$Frequency <29.0 & rfm_df_pokerChip$Frequency >= 5.0] <- 2
rfm_df_pokerChip$F_score[rfm_df_pokerChip$Frequency <5.0] <- 1

#Giving values of monetary to each customer
rfm_df_pokerChip$M_score <- 0
rfm_df_pokerChip$M_score[rfm_df_pokerChip$Monetary >= 1328.2] <- 4
rfm_df_pokerChip$M_score[rfm_df_pokerChip$Monetary < 1328.2 & rfm_df_pokerChip$Monetary >= 258.6] <- 3
rfm_df_pokerChip$M_score[rfm_df_pokerChip$Monetary >= 48.8 & rfm_df_pokerChip$Monetary < 258.6] <- 2
rfm_df_pokerChip$M_score[rfm_df_pokerChip$Monetary <48.8] <- 1

#Create the rfm score
rfm_df_pokerChip <- rfm_df_pokerChip %>% mutate(RFM_score_pokerChip = 100 *R_score +10 * F_score + M_score)

#segmenting to clients into 5 groups
rfm_df_pokerChip<-rfm_df_pokerChip%>%mutate(Loyalty_segmentation_pokerChip= case_when(
  between(RFM_score_pokerChip,0,178)~"Can’t Lose Them",
  between(RFM_score_pokerChip,179,244)~"At Risk Customers",
  between(RFM_score_pokerChip,245,311)~"New Customers",
  between(RFM_score_pokerChip,312,377)~"Potential Loyalists",
  TRUE~"Champions"))
#See results
head(rfm_df_pokerChip,20)
#Select columns in order to join
rfm_df_pokerChip <- rfm_df_pokerChip%>%
  select(UserID, RFM_score_pokerChip, Loyalty_segmentation_pokerChip)
summary(rfm_df_pokerChip)
###Graphic poker chip####

table(rfm_df_pokerChip$Loyalty_segmentation_pokerChip)
ggplot(rfm_df_pokerChip) + geom_bar(aes(x = Loyalty_segmentation_pokerChip, fill = Loyalty_segmentation_pokerChip))+theme(axis.text.x=element_text(angle=90,hjust=1)) +labs(title = "Barplot for Segments of customers")



#3.Create variables for poker
#Make the data frame of poker variables
poker_variables <- data.frame(UserID = DataMart$UserID, TransType = DataMart$TransType, TransAmount = DataMart$TransAmount, TransDatePoker = DataMart$TransDatePoker)
head(poker_variables)

PokerNew  <- poker_variables %>% group_by(UserID) %>%  
  summarise(MaxPokerTransDate = max(TransDatePoker),
            MinPokerTransDate = min(TransDatePoker),
            TotalPokerTranSell = sum(TransAmount[TransType==24]),
            TotalPokerTranBuy = sum(TransAmount[TransType==124]),
            MaxPokerTranSell = max(TransAmount[TransType==24]),
            MaxPokerTranBuy = max(TransAmount[TransType==124]),
            MinPokerTranSell = min(TransAmount[TransType==24]),
            MinPokerTranBuy = min(TransAmount[TransType==124]),
            AvgPokerTranSell = mean(TransAmount[TransType==24]),
            AvgPokerTranBuy = mean(TransAmount[TransType==124]),
            CountPokerTranSell = sum(TransType==24),
            CountPokerTranBuy = sum(TransType==124))
head(PokerNew)

#4.Number of products per each customer

Number_Products <- data.frame(UserID = DataMart$UserID, ProductID = DataMart$ProductID)
head(Number_Products)

Number_Products <- Number_Products %>%
  group_by(UserID) %>%
  summarise(NumberOfProducts = length(unique(ProductID, na.rm = TRUE)))

#5.1Creation of the total amount of stakes

total_stakes <- data.frame(UserID = DataMart$UserID, Stakes = DataMart$Stakes)
head(total_stakes)

total_stakes <- total_stakes %>%                              
  group_by(UserID) %>%
  summarise(Total_Stakes = sum(Stakes, na.rm = TRUE))

#5.2 Creation of the total amount of Winnings

total_winnings <- data.frame(UserID = DataMart$UserID, Winnings = DataMart$Winnings)
head(total_winnings)

total_winnings <- total_winnings %>%                              
  group_by(UserID) %>%
  summarise(Total_Winnings = sum(Winnings, na.rm = TRUE))

head(total_winnings)
sapply(total_winnings, class)

#5.3 Creation of the number of Bets

total_bets <- data.frame(UserID = DataMart$UserID, Bets = DataMart$Bets)

total_bets <- total_bets %>%
  group_by(UserID) %>%
  summarise(Total_Bets = sum(Bets, na.rm = TRUE))

head(total_bets)

#5.4 Merge the total_stakes and total_winnings
total_amount <- merge(total_stakes, total_winnings, "UserID")
head(total_amount)

#5.5 Merge the total total_amount and total_bets
total_amount <- merge(total_amount, total_bets, "UserID")
head(total_amount)      

#5.6 Create balance from total_amount
total_amount <- total_amount %>%
  mutate(final_balance = Total_Winnings - Total_Stakes)

#5.7 Creation of the number of days that each customer visited (Total active days per customer in the total period)
visited_days <- data.frame(UserID = DataMart$UserID, Date = DataMart$Date)
head(visited_days)

visited_days <- visited_days %>%                              
  group_by(UserID) %>%
  summarise(NumDaysOfVisited = n_distinct(Date))

#5.8 Merge total amount and visited_days
total_amount <- merge(total_amount, visited_days, 'UserID')
head(total_amount)


head(visited_days)

#7.Average of bets, winnings and stakes per visit
#Creation of Data frame
avg_total <- data.frame(UserID = DataMart$UserID, Winnings = DataMart$Winnings, Stakes = DataMart$Stakes, Bets = DataMart$Bets)

avg_total <- avg_total%>%
  group_by(UserID)%>%
  summarise(avg_winnings_per_visit = mean(Winnings),
            avg_stakes_per_visit = mean(Stakes),
            avg_bets_per_visit = mean(Bets))%>%
  mutate(ratio_win_stake = avg_winnings_per_visit/avg_stakes_per_visit)

head(avg_total)


############################## CREATION OF DATA MART MARKETING ##############################

#1.Creation of the data frame DataMartMarketing with the FirstActiveDate dataframe
DataMartMarketing <- data.frame(FirstActiveDate)
head(DataMartMarketing)



#2.Merging the LastActiveDate dataframe
DataMartMarketing <- merge(DataMartMarketing, LastActiveDate, "UserID")
head(DataMartMarketing)

sapply(DataMartMarketing, class)

#Creation of the column LOR: Long of Relationship which is the total days active
DataMartMarketing$LOR <- as.integer(
  difftime(DataMartMarketing$Last_Active_Date, DataMartMarketing$First_Active_Date, units = c("days"))
)

sapply(DataMartMarketing, class)
head(DataMartMarketing)
length(unique(DataMartMarketing$UserID))

#Merge the RFM of bettings in the DataMartMarketing (CHAGED)
DataMartMarketing <- merge(DataMartMarketing, rfm_betting, by = 'UserID', all.x = TRUE)

#Merge the RFM of pokerchip in the DataMartMarketing (CHAGED)
DataMartMarketing <- merge(DataMartMarketing, rfm_df_pokerChip, by = 'UserID', all.x = TRUE)

#3.Adding PokerNew variables in the DataMartMarketing
DataMartMarketing <- merge(DataMartMarketing, PokerNew, 'UserID')

#4.Adding the Number of Products variable to the DataMartMarketing
DataMartMarketing <- merge(DataMartMarketing, Number_Products, 'UserID')

#5.Adding the Total amount dataframe to DataMartMarketing
DataMartMarketing <- merge(DataMartMarketing, total_amount, 'UserID')
head(DataMartMarketing)

#6. Join the avg_total data frame
DataMartMarketing <- merge(DataMartMarketing, avg_total, 'UserID')

#Joinning the DataI_Demo to DataMartMarketing (CHAGED)
DataMartMarketing <- merge(DataMartMarketing, DataI_Demo, by = 'UserID', all.x = TRUE)

glimpse(DataMartMarketing)

#########ADDING THE APPENDIX TO DATA MART MARKETING######################

#Adding the column Country
#Joinning the Appendix 2 to the DataMartMarketing: The Country with the Country Name
DataMartMarketing <- DataMartMarketing %>% left_join(AppCountry, by='Country')%>% rename(Country_Name = `Country Name`)

#Adding the column Language
#Joinning the Appendix 3 to the DataMartMarketing: The Language with the Language Description
DataMartMarketing <- DataMartMarketing %>% left_join(AppLanguage, by='Language')%>% rename(Language_Description = `Language Description`)

#Adding the column ApplicationID
#Joinning the Appendix 4 to the DataMartMarketing: The ApplicationID with the Application Description
DataMartMarketing <- DataMartMarketing %>% left_join(AppApplication, by='ApplicationID')%>%rename(Application_Description = `Application Description`)

sapply(DataMartMarketing, class)

#############ORDERING THE COLUMNS OF DATA MART MARKETING#################

DataMartMarketing <- DataMartMarketing %>% select( "UserID","Gender", "Country_Name", "Country", "Language_Description", "Language", "Application_Description", "ApplicationID", "NumberOfProducts", everything())
head(DataMartMarketing)
str(DataMartMarketing)
############GENERAL INFORMATION #######
#Number of customers
print(length(unique(DataMart$UserID)))

#Number of countries
print(length(unique(DataMartMarketing$Country_Name)))

#Number of languages of the customers
print(length(unique(DataMart$Language_Description)))

#Highest buy amount in poker
print(max(DataMartMarketing$MaxPokerTranBuy[is.finite(DataMartMarketing$MaxPokerTranBuy)]))

#Highest sell amount in poker
print(max(DataMartMarketing$MaxPokerTranSell[is.finite(DataMartMarketing$MaxPokerTranSell)]))

#Highest amount of winnings
print(max(DataMartMarketing$Total_Winnings[is.finite(DataMartMarketing$Total_Winnings)]))

#Highest amount of stakes
print(max(DataMartMarketing$Total_Stakes[is.finite(DataMartMarketing$Total_Stakes)]))

#Max number of bets
print(max(DataMartMarketing$Total_Bets[is.finite(DataMartMarketing$Total_Bets)]))

############SOME GRAPHICS ######
#Inspect the na values of all the columns
na_count <-sapply(DataMartMarketing, function(y) sum(length(which(is.na(y)))))
na_count

#Plot 1
# %Segments in Bettings
Percent_Segments_Bettings <- DataMartMarketing%>%
  filter(!is.na(Loyalty_segmentation_bettings))%>%
  group_by(Loyalty_segmentation_bettings)%>%
  summarise(count = n())%>%
  mutate(counTotal = sum(count),
         Percent = 100*count/counTotal)%>%
  arrange(desc(Percent))
Percent_Segments_Bettings

#Plot 2
# %Segments in Poker
Percent_Segments_Poker <- DataMartMarketing%>%
  filter(!is.na(Loyalty_segmentation_pokerChip))%>%
  group_by(Loyalty_segmentation_pokerChip)%>%
  summarise(count = n())%>%
  mutate(counTotal = sum(count),
         Percent = 100*count/counTotal)%>%
  arrange(desc(Percent))

Percent_Segments_Poker

#Plot 3
#Graphic about the gender

#Graphic Gender 1
DataMartMarketing%>%
  ggplot()+geom_bar(aes(x=Gender, fill = Gender))

#Graphic Gender 2
graph_gender <- DataMartMarketing%>%
  group_by(Gender)%>%
  summarise(Count = n())%>%
  ungroup()%>%
  mutate(Gender = factor(Gender, levels = c("Female", "Male")))%>%
  ggplot(aes(x=Gender, y=Count))+
  geom_segment(aes(xend=Gender, yend=0)) +
  geom_point( size=9, color="orange") +
  theme_bw() +
  xlab("")+
  ylab("Frecuency")+
  labs(title = "Gender distribution of users")+
  theme(plot.title = element_text(hjust = 0.5))

graph_gender

#Plot 4
#Graphic country customers by gender

graph_genderBycountry <- DataMartMarketing%>%
  group_by(Country_Name, Gender)%>%
  summarise(Count = n())%>%
  arrange(Country_Name)%>%
  filter(Count>100)%>%
  ungroup()%>%
  ggplot(aes(x=Count, y=Country_Name)) + 
  geom_bar(position = "dodge",stat = "identity", aes(fill=Gender)) +
  labs(title= "Country customers by gender",
       x = "Customers",
       y = "Country and Gender") 

graph_genderBycountry

#Plot 5
#Graphic about the countries
library(forcats)

graph_countries <-     DataMartMarketing%>%
  group_by(Country_Name)%>%
  summarise(NCountry = n())%>%
  arrange(desc(NCountry))%>%
  slice(1:10)%>%
  ungroup()%>%
  mutate(Country_Name = fct_reorder(Country_Name, NCountry))%>%
  ggplot(aes(x=Country_Name, y=NCountry))+
  geom_segment( aes(xend=Country_Name, yend=0)) +
  geom_point( size=6, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Frecuency")+
  labs(title = "Top 10 country distribution of users")+
  theme(plot.title = element_text(hjust = 0.5))

graph_countries

#Plot 6
#Graphic about the language

graph_language <-     DataMartMarketing%>%
  group_by(Language_Description)%>%
  summarise(Count = n())%>%
  arrange(desc(Count))%>%
  slice(1:17)%>%
  ungroup()%>%
  mutate(Language_Description = fct_reorder(Language_Description, Count))%>%
  ggplot(aes(x=Language_Description, y=Count))+
  geom_segment( aes(xend=Language_Description, yend=0)) +
  geom_point( size=6, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Frecuency")+
  labs(title = "Language distribution of users")+
  theme(plot.title = element_text(hjust = 0.5))

graph_language

#Plot 7
#Graphic segmentation clients in bettings
#This graphic describe the distribution of the segmentation clients in bettings
table(DataMartMarketing$Loyalty_segmentation_bettings)


DataMartMarketing%>%
  arrange(Country_Name)%>%
  filter(!is.na(Loyalty_segmentation_bettings))%>%
  ggplot() + geom_bar(aes(x = factor(Loyalty_segmentation_bettings, level = c('Can’t Lose Them',"At Risk Customers","New Customers","Potential Loyalists","Champions")),
                          fill = Loyalty_segmentation_bettings),
                      fill="lightblue",width=0.2)+
  theme_bw() +
  coord_flip() +
  theme(axis.text.x=element_text(angle=0,hjust=1), legend.position="none") +
  scale_fill_hue(c = 40) +
  xlab("")+
  ylab("Frecuency") +
  labs(title = "Loyalty Segmentation of Bettings")+
  theme(plot.title = element_text(hjust = 0.5))

#Plot 8
#Graphic segmentation clients in poker
#This graphic describe the distribution of the segmentation clients in poker chip

  DataMartMarketing%>%
    arrange(Country_Name)%>%
    filter(!is.na(Loyalty_segmentation_pokerChip))%>%
    ggplot() + geom_bar(aes(x = factor(Loyalty_segmentation_pokerChip, level = c('Can’t Lose Them',"At Risk Customers","New Customers","Potential Loyalists","Champions"))
                            , fill = Loyalty_segmentation_pokerChip),fill="orange",width=0.2) +
    theme_bw() +
    coord_flip() +
    theme(axis.text.x=element_text(angle=0,hjust=1), legend.position="none") +
    scale_fill_hue(c = 40) +
    xlab("")+
    ylab("Frecuency") +
    labs(title = "Loyalty Segmentation of Poker")+
    theme(plot.title = element_text(hjust = 0.5))


#Plot 9
#Graphic about the first active day by gender
first_active_day_gender <-     DataMartMarketing%>%
  mutate(day = day(First_Active_Date))%>%
  group_by(Gender, day)%>%
  summarise(count=n())%>%
  ggplot()+
  geom_line(aes(x=day,y=count, color = Gender))+
  xlim(c(1,30))+
  theme_bw()+
  xlab("Days") +
  ylab("Frecuency") +
  labs(title = "First active day distribution by gender")+
  theme(plot.title = element_text(hjust = 0.5))
  
first_active_day_gender 
  #group_by(Gender)

#Plot 10
#Graphic of frecuency of use the aplications
graph_app <- DataMartMarketing%>%
  group_by(Application_Description)%>%
  summarise(NApp = n())%>%
  arrange(desc(NApp))%>%
  slice(1:10)%>%
  ungroup()%>%
  mutate(Application_Description = fct_reorder(Application_Description, NApp))%>%
  ggplot(aes(x = Application_Description, y= NApp)) + 
  geom_bar(stat="identity", position="identity",fill="orange",width=0.2)+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Barplot for Application")+
  theme_bw() +
  coord_flip()+
  xlab("Application") +
  ylab("Frecuency")
  

graph_app

#Export data into a RData
save(DataMartMarketing,file="C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\DataMartMarketing.RData")

#Export data into a csv
write.table(DataMartMarketing, 
            file = "C:\\Users\\bariastoribio\\OneDrive - IESEG\\IESEG 1_SEMESTER\\Business Analytics Tools_Open Source_R\\Group assignment\\Group Assignement_Files\\DataMartMaketing.csv", 
            row.names = F, 
            sep=",")
