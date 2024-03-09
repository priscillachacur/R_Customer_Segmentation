#' Title: A1 Direct Mail Household EDA
#' Purpose: Getting insights for BBY
#' Analyzing data for BBY Loyalty Customers for Retention
#' Author: Priscilla Chacur
#' Date: March 2023

# libraries
library(stringr)
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(powerjoin)
library(scales)


# Set working directory
setwd("~/Desktop/Data Visualization with R/hult_R_class/personalFiles")

#Getting rid of scientific notation
# Options
options(scipen = 999)

#Getting each data file individually
consumerDataDF <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1/consumerData_training15K_studentVersion.csv')
DonationsDataDF <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1/DonationsData_training15K_studentVersion.csv')
inHouseDataDF <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1/inHouse_EDA_10k.csv')
magazineDataDF <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1/magazineData_training15K_studentVersion.csv')
politicalDataDF <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1/politicalData_training15K_studentVersion.csv')

#################################################################################
###################### BEGINNING OF INDIVIDUAL DATA EDA #########################
#################################################################################

#Exploring the each data file individually (goal: get familiar with the data)

############ConsumerData############
names(consumerDataDF)

#Understanding the data
summary(consumerDataDF)
str(consumerDataDF)
dim(consumerDataDF)
sapply(consumerDataDF,class)
#Finding how many unique values are there in Median Education Years
nlevels( as.factor(consumerDataDF$MedianEducationYears))
#Finding how many unique values are there in ISPSA
nlevels( as.factor(consumerDataDF$ISPSA))
# What's the correlation between ISPSA and MedianEducationYears (ignoring missing values)
cor(consumerDataDF$ISPSA,consumerDataDF$MedianEducationYears, use = 'complete.obs')
     #Result: 0.0144 (Interpreted as very weak positive correlation)
# What are the unique HomeOwnerRenter Values
unique(consumerDataDF$HomeOwnerRenter)
    #This shows that the data for HomeOwnerRenter needs cleaning (since there are nulls)
#Finding mean for ISPSA (rate between 0 - 9)
mean(consumerDataDF$ISPSA, na.rm = T) 
      #Result: 6.89
#Relationship between ISPSA & ResidenceHHGenderDescription
ggplot(consumerDataDF) +
  geom_bar(aes(x = ISPSA, fill = ResidenceHHGenderDescription))
  #Mixed gender household is the majority for all ISPSA
#Changing the class type to see the relationship between them
consumerDataDF$newISPSA <- as.factor(consumerDataDF$ISPSA)
    #Relationship between ResidenceHHGenderDescription & ISPSA
    ggplot(consumerDataDF) +
        geom_bar(aes(x = ResidenceHHGenderDescription, fill = newISPSA))
    #Result does not provide visually significant insights
#Understanding the % of data missing in each column
    #Creating temp list to get missing values
    tmp <- list()
    for(i in 1:ncol(consumerDataDF)){
      print(i)
      x <- consumerDataDF[,i]
      x <- as.character(x)  # convert to character vector
      x <- ifelse(nchar(x)==0,NA,x)
      tmp[[i]] <- x
    }
    tmp <- do.call(cbind, tmp)
    colnames(tmp) <- names(consumerDataDF)
    tmp <- as.data.frame(tmp)
    plot_missing(tmp)
    #9 variables with above 80% of missing values (might be irrelevant for the analysis)

############DonationsDataDF############
names(DonationsDataDF)

#Understanding the data
summary(DonationsDataDF)
str(DonationsDataDF)
dim(DonationsDataDF)
sapply(DonationsDataDF,class)
# What are the unique ReligiousContributorInHome?
unique(DonationsDataDF$ReligiousContributorInHome)

############inHouseDataDF############
names(inHouseDataDF)

#Understanding the data
summary(inHouseDataDF)
str(inHouseDataDF)
dim(inHouseDataDF)
sapply(inHouseDataDF,class)
#Relationship between PropertyType & storeVisitFrequency
  #Getting x axis as property type and Store Visit Frequency as stacked variables
  #Changing the data class to be able to plot
  inHouseDataDF$newStoreSF <- as.factor(inHouseDataDF$storeVisitFrequency)
    ggplot(inHouseDataDF, aes(x = PropertyType, fill = newStoreSF)) +
      geom_bar() +
      labs(title = "Relationship between PropertyType and storeVisitFrequency", 
       x = "PropertyType", y = "Count")
  #Getting x axis as Store Visit Frequency and property type as stacked variables
  ggplot(inHouseDataDF, aes(x = storeVisitFrequency, fill = PropertyType)) +
      geom_bar() +
      labs(title = "Relationship between PropertyType and storeVisitFrequency", 
         x = "StoreVisitFrequency", y = "Count")

############magazineDataDF############
names(magazineDataDF)
  
#Understanding the data
summary(magazineDataDF)
str(magazineDataDF)
dim(magazineDataDF)
sapply(magazineDataDF,class)
#Finding how many unique values are there in FinancialMagazineInHome
nlevels( as.factor(magazineDataDF$FinancialMagazineInHome))
#Finding how many unique values are there in HealthFitnessMagazineInHome
nlevels( as.factor(magazineDataDF$HealthFitnessMagazineInHome))
# What are the unique HealthFitnessMagazineInHome Values
unique(magazineDataDF$HealthFitnessMagazineInHome)

############politicalDataDF############
names(politicalDataDF)

#Understanding the data
summary(politicalDataDF)
str(politicalDataDF)
dim(politicalDataDF)
sapply(politicalDataDF,class)
#Finding how many unique values are there in ReligionsDescription
nlevels( as.factor(politicalDataDF$ReligionsDescription))
#Finding how many unique values are there in PartiesDescription
nlevels( as.factor(politicalDataDF$PartiesDescription))
# What are the unique ReligionsDescription Values
unique(politicalDataDF$ReligionsDescription)
#Relationship between Parties Description & ReligionsDescription
#Getting x axis as Parties Description and Religions Description as stacked variables
ggplot(politicalDataDF, aes(x = PartiesDescription, fill = ReligionsDescription)) +
  geom_bar() +
  labs(title = "Relationship between PropertyType and storeVisitFrequency", 
       x = "StoreVisitFrequency", y = "Count")

#################################################################################
###################### END OF INDIVIDUAL DATASETS EDA ###########################
#################################################################################


#################################################################################
###################### BEGINNING OF MERGED DATASET EDA ##########################
#################################################################################

#Getting the data
folderPath <- '~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A1'

#Getting all csv files in a folder (reg expression with .csv)
# Multiple files as a list
tmp <- list.files(path       = folderPath, 
                  pattern    = '*.csv',
                  full.names = T)

#List apply function --> reading all csv files
trainingdata <- lapply(tmp, read.csv)

# Using merge to put all the data together
households <- power_left_join(trainingdata, by = "tmpID")

# Filtering only for the loyalty program customers
# Use complete.cases() function
## Goal: Understanding the loyalty customers to use for retention purposes
bbyLoyalty <- households[complete.cases(households),]

######################## UNDERSTANDING BBY LOYALTY MEMBERS ######################## 
# Basic Exploration
names(bbyLoyalty)

#Top 6 rows
head(bbyLoyalty)

#Understanding the data
summary(bbyLoyalty)
str(bbyLoyalty)
dim(bbyLoyalty)
sapply(bbyLoyalty,class)
# What's the correlation between ISPSA and storeVisitFrequency (ignoring missing values)
cor(bbyLoyalty$ISPSA,bbyLoyalty$storeVisitFrequency, use = 'complete.obs')
#Result: -0.0161 (Interpreted as very weak negative correlation)

#Understanding the % of data missing in each column
#Creating temp list to get missing values
tmp <- list()
for(i in 1:ncol(bbyLoyalty)){
  print(i)
  x <- bbyLoyalty[,i]
  x <- as.character(x)  # convert to character vector
  x <- ifelse(nchar(x)==0,NA,x)
  tmp[[i]] <- x
}
tmp <- do.call(cbind, tmp)
colnames(tmp) <- names(bbyLoyalty)
tmp <- as.data.frame(tmp)
#Plot graph
plot_missing(tmp)
ggsave("allvariables_missing.png")
  #Result is too hard to visualize 

# Filter columns based on missing value proportions
missing_prop <- colMeans(is.na(tmp))
tmp_filtered <- tmp[, missing_prop > 0.4]
plot_missing(tmp_filtered, title = "Proportion of missing values > 40%")
  #Result is a significant number of variables have 
#saving the imagine programmatically
ggsave("missingvalues_40.png")

# Removing columns for 80% rules
tmp_missing_80 <- tmp[, missing_prop > 0.8]
# Create a list of column names with missing value proportions above 80%
cols_to_delete <- names(tmp_missing_80)
# Remove the columns from the original data frame
bbyLoyalty <- bbyLoyalty[, !names(bbyLoyalty) %in% cols_to_delete]

# Dimensions with tmp_missing_80 removed
dim(bbyLoyalty)
#Result: 9678 obs 49 variables

summary(bbyLoyalty)
sapply(bbyLoyalty,class)

#Overall view on the dataset
plot_histogram(bbyLoyalty)

#Store Visit Frequency
plot_histogram(bbyLoyalty$storeVisitFrequency)

#Refreshing the missing data
  #Creating temp list to get missing values
tmp <- list()
for(i in 1:ncol(bbyLoyalty)){
  print(i)
  x <- bbyLoyalty[,i]
  x <- as.character(x)  # convert to character vector
  x <- ifelse(nchar(x)==0,NA,x)
  tmp[[i]] <- x
}
tmp <- do.call(cbind, tmp)
colnames(tmp) <- names(bbyLoyalty)
tmp <- as.data.frame(tmp)
    #Plot graph
    plot_missing(tmp)

#Checking all 49 variables
names(bbyLoyalty)

#Filling in for LandValue
bbyLoyalty$LandValue
#Moving the $ of the data
bbyLoyalty$LandValue <- sub("\\$", "", bbyLoyalty$LandValue)
#Changing class type
bbyLoyalty$LandValue <- as.numeric(bbyLoyalty$LandValue)
#Can't fill in with 0 because the assumption is a missing value and not that the land value = 0. So we get the mean
mean(bbyLoyalty$LandValue, na.rm = TRUE)
#Result: Mean= 14055.46
#Fill in the NA with the mean rounded to the nearest integer: 173561
bbyLoyalty$LandValue <- ifelse(is.na(bbyLoyalty$HomePurchasePrice), round(mean(bbyLoyalty$HomePurchasePrice, na.rm = TRUE)), bbyLoyalty$HomePurchasePrice)
bbyLoyalty$LandValue

#Relationship between HomePurchasePrice and LandValue
    #cor(bbyLoyalty$HomePurchasePrice,bbyLoyalty$LandValue, use = 'complete.obs')
    #Result: 1 (Strong Positive Correlation)
    #Therefore no point in keeping both - decided to keep only HomePurchasePrice

#List for variables that are not part of the analysis
irrelevant_variables <- list('LastName','FirstName','MosaicZ4','lat','lon','county',
                             'fips','stateFips','TelephonesFullPhone','GunOwner',
                             'PoliticalContributerInHome','ReligiousContributorInHome',
                             'city','BroadEthnicGroupings','ComputerOwnerInHome',
                             'DonatestoHealthcare','DonatestoHealthcare1','DonatesEnvironmentCauseInHome',
                             'EstHomeValue','LandValue') 
#Removing everything in the irrelevant_variables list
bbyLoyalty <- bbyLoyalty[, !names(bbyLoyalty) %in% irrelevant_variables]


#List of variables after removal of variables
names(bbyLoyalty)

#Understanding the missing values variables for data cleaning
#Creating temp list to get missing values
tmp <- list()
for(i in 1:ncol(bbyLoyalty)){
  print(i)
  x <- bbyLoyalty[,i]
  x <- as.character(x)  # convert to character vector
  x <- ifelse(nchar(x)==0,NA,x)
  tmp[[i]] <- x
}
tmp <- do.call(cbind, tmp)
colnames(tmp) <- names(bbyLoyalty)
tmp <- as.data.frame(tmp)
#Plot graph
plot_missing(tmp, title = "Variables with % of missing values")
#saving the imagine programmatically
ggsave("32variables_missing.png")


### Dealing with missing values ###

#List of variables with missing values > 50%
#HomePurchasePrice, Investor,BookBuyerInHome, DonatestoLocalCommunity
#HealthFitnessMagazineInHome,GeneralCollectorinHousehold, FamilyMagazineInHome,
#InterestinCurrentAffairsPoliticsInHousehold, BuyerofArtinHousehold

#### Filling in the nulls #### 

#BuyerofArtinHousehold
bbyLoyalty$BuyerofArtinHousehold
# Replace similar values in the education with the same value
bbyLoyalty$BuyerofArtinHousehold <- ifelse(bbyLoyalty$BuyerofArtinHousehold == "", 0, 1)
bbyLoyalty$BuyerofArtinHousehold
class(bbyLoyalty$BuyerofArtinHousehold)

#InterestinCurrentAffairsPoliticsInHousehold
bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold
bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold <- ifelse(bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold == "", 0, 1)
bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold
class(bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold)

#FamilyMagazineInHome
bbyLoyalty$FamilyMagazineInHome
bbyLoyalty$FamilyMagazineInHome <- gsub("family-oriented magazine purchases|family-oriented magazine purchase", "", bbyLoyalty$FamilyMagazineInHome)
bbyLoyalty$FamilyMagazineInHome <- ifelse(bbyLoyalty$FamilyMagazineInHome == "", 0,bbyLoyalty$FamilyMagazineInHome )
bbyLoyalty$FamilyMagazineInHome <- as.numeric(bbyLoyalty$FamilyMagazineInHome)
class(bbyLoyalty$FamilyMagazineInHome)


#GeneralCollectorinHousehold
bbyLoyalty$GeneralCollectorinHousehold
bbyLoyalty$GeneralCollectorinHousehold <- ifelse(bbyLoyalty$GeneralCollectorinHousehold == "", 0, 1)
bbyLoyalty$GeneralCollectorinHousehold
class(bbyLoyalty$GeneralCollectorinHousehold)

#HealthFitnessMagazineInHome
bbyLoyalty$HealthFitnessMagazineInHome
bbyLoyalty$HealthFitnessMagazineInHome <- gsub("health and fitness magazine purchases|health and fitness magazine purchase", "", bbyLoyalty$HealthFitnessMagazineInHome)
bbyLoyalty$HealthFitnessMagazineInHome <- ifelse(bbyLoyalty$HealthFitnessMagazineInHome == "", 0,bbyLoyalty$HealthFitnessMagazineInHome )
bbyLoyalty$HealthFitnessMagazineInHome <- as.numeric(bbyLoyalty$HealthFitnessMagazineInHome)
class(bbyLoyalty$HealthFitnessMagazineInHome)

#DonatestoLocalCommunity
bbyLoyalty$DonatestoLocalCommunity
bbyLoyalty$DonatestoLocalCommunity <- ifelse(bbyLoyalty$DonatestoLocalCommunity == "", 0, 1)
bbyLoyalty$DonatestoLocalCommunity
class(bbyLoyalty$DonatestoLocalCommunity)

#BookBuyerInHome
bbyLoyalty$BookBuyerInHome
bbyLoyalty$BookBuyerInHome <- gsub("book purchase in home|book purchases in home", "", bbyLoyalty$BookBuyerInHome)
bbyLoyalty$BookBuyerInHome <- ifelse(bbyLoyalty$BookBuyerInHome == "", 0,bbyLoyalty$BookBuyerInHome )
bbyLoyalty$BookBuyerInHome <- as.numeric(bbyLoyalty$BookBuyerInHome)
class(bbyLoyalty$BookBuyerInHome)

#Investor
bbyLoyalty$Investor
bbyLoyalty$Investor <- ifelse(bbyLoyalty$Investor == "", 0, 1)
bbyLoyalty$Investor
class(bbyLoyalty$Investor)

#HomePurchasePrice
bbyLoyalty$HomePurchasePrice
  #Moving the $ of the data
bbyLoyalty$HomePurchasePrice <- sub("\\$", "", bbyLoyalty$HomePurchasePrice)
  #Changing class type
bbyLoyalty$HomePurchasePrice <- as.numeric(bbyLoyalty$HomePurchasePrice)
  #Can't fill in with 0 because the assumption is a missing value and not that the home purchase price = 0. So we get the mean
mean(bbyLoyalty$HomePurchasePrice, na.rm = TRUE)
  #Result: Mean= 173560.7
  #Fill in the NA with the mean rounded to the nearest integer: 173561
bbyLoyalty$HomePurchasePrice <- ifelse(is.na(bbyLoyalty$HomePurchasePrice), round(mean(bbyLoyalty$HomePurchasePrice, na.rm = TRUE)), bbyLoyalty$HomePurchasePrice)
bbyLoyalty$HomePurchasePrice


#Rerunning the percentage of missing values per variable
tmp <- list()
for(i in 1:ncol(bbyLoyalty)){
  print(i)
  x <- bbyLoyalty[,i]
  x <- as.character(x)  # convert to character vector
  x <- ifelse(nchar(x)==0,NA,x)
  tmp[[i]] <- x
}
tmp <- do.call(cbind, tmp)
colnames(tmp) <- names(bbyLoyalty)
tmp <- as.data.frame(tmp)
#Plot graph
plot_missing(tmp)

#List of variables with missing values to organize it
#ReligionsDescription, LandValue, NetWorth, DwellingUnitSize,
#PresenceOfChildrenCode,EthnicDescription,HomeOwnerRenter

#ReligionsDescription
bbyLoyalty$ReligionsDescription
bbyLoyalty$ReligionsDescription <- ifelse(nchar(bbyLoyalty$ReligionsDescription) == 0, "Unknown", bbyLoyalty$ReligionsDescription)
bbyLoyalty$ReligionsDescription


#DwellingUnitSize
bbyLoyalty$DwellingUnitSize
bbyLoyalty$DwellingUnitSize <- gsub("-Single Family Dwelling|-Triplex |-Duplex", "", bbyLoyalty$DwellingUnitSize)
bbyLoyalty$DwellingUnitSize <- ifelse(bbyLoyalty$DwellingUnitSize == "", 0,bbyLoyalty$DwellingUnitSize )
unique(bbyLoyalty$DwellingUnitSize)
  #to find out how many values are in each category
  table(bbyLoyalty$DwellingUnitSize)
bbyLoyalty$DwellingUnitSize

#PresenceOfChildrenCode
bbyLoyalty$PresenceOfChildrenCode
table(bbyLoyalty$PresenceOfChildrenCode)
  #Substituting the missing value to unknown
  bbyLoyalty$PresenceOfChildrenCode <- ifelse(nchar(bbyLoyalty$PresenceOfChildrenCode) == 0, "Unknown", bbyLoyalty$PresenceOfChildrenCode)
unique(bbyLoyalty$PresenceOfChildrenCode)

#EthnicDescription
bbyLoyalty$EthnicDescription
unique(bbyLoyalty$EthnicDescription)
bbyLoyalty$EthnicDescription <- ifelse(nchar(bbyLoyalty$EthnicDescription) == 0, "Unknown", bbyLoyalty$EthnicDescription)
bbyLoyalty$EthnicDescription

#HomeOwnerRenter
bbyLoyalty$HomeOwnerRenter
bbyLoyalty$HomeOwnerRenter <- gsub("Likely", "", bbyLoyalty$HomeOwnerRenter)
bbyLoyalty$HomeOwnerRenter
table(bbyLoyalty$HomeOwnerRenter)
  #Missing values, we are keeping as unknwon
  bbyLoyalty$HomeOwnerRenter <- ifelse(nchar(bbyLoyalty$HomeOwnerRenter) == 0, "Unknown", bbyLoyalty$HomeOwnerRenter)
bbyLoyalty$HomeOwnerRenter

#Gender
bbyLoyalty$Gender
table(bbyLoyalty$Gender)
  #Only 1 missing value that is being substituted for F which is the majority
  bbyLoyalty$Gender <- ifelse(nchar(bbyLoyalty$Gender) == 0, "F", bbyLoyalty$Gender)
bbyLoyalty$Gender
  
############ Networth ########
#NetWorth
bbyLoyalty$NetWorth
  class(bbyLoyalty$NetWorth)
  #getting the most frequent networth to input for missing values
  most_freq_range <- names(sort(table(bbyLoyalty$NetWorth), decreasing = TRUE))[1]
  most_freq_range
  #most_freq_range = "$100000-249999"
  #Inputting the most_freq_range for missing values
  bbyLoyalty$NetWorth <- ifelse(nchar(bbyLoyalty$NetWorth) == 0, most_freq_range, bbyLoyalty$NetWorth)
  table(bbyLoyalty$NetWorth)
  bbyLoyalty$NetWorth <- sub("\\$", "", bbyLoyalty$NetWorth)
    # Chg to factor for ggplot
    bbyLoyalty$NetWorth <- factor(bbyLoyalty$NetWorth) 
    #plot graph for frequency
    ggplot(data = data.frame(NetWorth = bbyLoyalty$NetWorth), aes(x = NetWorth)) +
     geom_bar(fill = 'darkred') +
     coord_flip() +
     theme_gdocs() +
    geom_text(stat = 'count', aes(label = ..count..), colour = "white", hjust = 1.25, size = 5.0)
    
#Converting values to categorical

#DonatesToCharityInHome
bbyLoyalty$DonatesToCharityInHome
bbyLoyalty$DonatesToCharityInHome <- ifelse(bbyLoyalty$DonatesToCharityInHome == "Yes", 1, 0)
bbyLoyalty$DonatesToCharityInHome
class(bbyLoyalty$DonatestoLocalCommunity)   


#Education
bbyLoyalty$Education
bbyLoyalty$Education <- gsub("- Likely | - Extremely Likely|-Extremely Likely|-Likely| - Likely| - Ex Like",
                             "", bbyLoyalty$Education)
bbyLoyalty$Education <- gsub("Some College ", "Some College", bbyLoyalty$Education)
bbyLoyalty$Education
table(bbyLoyalty$Education)

#Age
bbyLoyalty$Age
bbyLoyalty$Age <- round(bbyLoyalty$Age)
bbyLoyalty$Age

#State
bbyLoyalty$state
table(bbyLoyalty$state)
most_freq_state <- names(sort(table(bbyLoyalty$state), decreasing = TRUE))[1]
  #Result is Texas
bbyLoyalty$state <- sub("NULL", "Texas", bbyLoyalty$state)
table(bbyLoyalty$state)

#y_householdSpend
bbyLoyalty$y_householdSpend
bbyLoyalty$y_householdSpend <- round(bbyLoyalty$y_householdSpend)
bbyLoyalty$y_householdSpend

#Veteran
bbyLoyalty$Veteran
bbyLoyalty$Veteran <- ifelse(bbyLoyalty$Veteran == "Yes", 1, 0)
bbyLoyalty$Veteran
class(bbyLoyalty$Veteran) 

#PresenceOfChildrenCode
bbyLoyalty$PresenceOfChildrenCode
bbyLoyalty$PresenceOfChildrenCode <- gsub("Modeled ", "", bbyLoyalty$PresenceOfChildrenCode)
bbyLoyalty$PresenceOfChildrenCode <- sub("Not as Likely to have a child", "Not Likely to have a child", bbyLoyalty$PresenceOfChildrenCode)
bbyLoyalty$PresenceOfChildrenCode
table(bbyLoyalty$PresenceOfChildrenCode)


############################# Clean Data ################################### 
    
head(bbyLoyalty)
#Check for nulls
colSums(is.na(bbyLoyalty))    

############### Finding relationship between variables #################### 

#List of variables
variable_list <- names(bbyLoyalty)
variable_list
#Result:
  #tmpID, DonatesToCharityInHome                    
  #DonatestoLocalCommunity,ResidenceHHGenderDescription"               
  #EthnicDescription",PresenceOfChildrenCode"                     
  #ISPSA","HomeOwnerRenter"                            
  #MedianEducationYears","NetWorth"                                   
  #Investor","Education"                                  
  #OccupationIndustry","BookBuyerInHome"                            
  #BuyerofArtinHousehold","GeneralCollectorinHousehold"                
  #Gender","Age"                                        
  #state","HomePurchasePrice",                          
  #"DwellingUnitSize"                           
  #"storeVisitFrequency","PropertyType"                               
  #"y_householdSpend","FamilyMagazineInHome"                       
  #"HealthFitnessMagazineInHome","InterestinCurrentAffairsPoliticsInHousehold"
  #"PartiesDescription","ReligionsDescription"                       
  #"Veteran"

#Dimensions
dim(bbyLoyalty)

#Overall visualization (Using SUBSETING FUNCTION)
  # Define the list of numerical variables
  numerical_variables <- c("ISPSA", "MedianEducationYears", "BookBuyerInHome",
                           "Age",'HomePurchasePrice','DwellingUnitSize',
                           'storeVisitFrequency','y_householdSpend','FamilyMagazineInHome',
                           'HealthFitnessMagazineInHome')
  # Subset the bbyLoyalty dataset to only include the numerical variables
  bbyLoyalty_numeric <- subset(bbyLoyalty, select = numerical_variables)
  # Plot the histogram using the subseted dataset
  plot_histogram(bbyLoyalty_numeric, title = "Overall Visualization of Numeric Data")
  ggsave("numeric_data.png")

#Correlation between Age and y_householdSpend
cor(bbyLoyalty$Age,bbyLoyalty$y_householdSpend, use = 'complete.obs')
  #Result -0.0114 (very weak negative correlation)

#Correlation between HomePurchasePrice and BookBuyerInHome
cor(bbyLoyalty$HomePurchasePrice,bbyLoyalty$BookBuyerInHome, use = 'complete.obs')
  #Result: -0.00291 (very weak negative correlation)

#Correlation between HomePurchasePrice and MedianEducationYears
cor(bbyLoyalty$HomePurchasePrice,bbyLoyalty$MedianEducationYears, use = 'complete.obs')
  #Result: 0.0113 (very weak positive correlation)

#Correlation between Age and y_householdSpend
cor(bbyLoyalty$Age,bbyLoyalty$y_householdSpend, use = 'complete.obs')
  #Result: -0.0114 (very weak negative correlation)

#Visual relationship between PropertyType & ResidenceHHGenderDescription
ggplot(bbyLoyalty) +
  geom_bar(aes(x = PropertyType, fill = ResidenceHHGenderDescription))

#Visual relationship between Gender & NetWorth
  ggplot(bbyLoyalty) +
    geom_bar(aes(x = NetWorth, fill = Gender))

#Visual relationship between ISPSA & ResidenceHHGenderDescription
  ggplot(bbyLoyalty) +
    geom_bar(aes(x = ISPSA, fill = ResidenceHHGenderDescription))

#Correlation between ISPSA and storeVisitFrequency
  cor(bbyLoyalty$ISPSA,bbyLoyalty$storeVisitFrequency, use = 'complete.obs')
  #Result: -0.016 (very weak negative correlation)

############ INSIGHT 1 ################

#From a range of Home Purchasing Price --> checking for educational level and kids
  
#Looking into the Home Purchasing Price
plot_histogram(bbyLoyalty$HomePurchasePrice)
  #Clearly is possible to see a significant peak for a specific value
  table(bbyLoyalty$HomePurchasePrice)
  #Using Subsetting function to limit the data
  Homepurchaseprice_subset <- subset(bbyLoyalty, bbyLoyalty$HomePurchasePrice >= 170000 & bbyLoyalty$HomePurchasePrice <= 175000)
  head(Homepurchaseprice_subset$HomePurchasePrice)
    #Result: 173561 173561 173561 173561 173561 173561
  
#Subsetting the Home Purchasing Price for those in the range (173561 173561 173561 173561 173561 173561)

#Visual relationship between PresenceOfChildrenCode & Education
  ggplot(bbyLoyalty) +
    geom_bar(aes(x = PresenceOfChildrenCode, fill = Education))

#PresenceOfChildrenCode & Education filtered by the homepurchaseprice_subset
  ggplot(Homepurchaseprice_subset) +
    geom_bar(aes(x = PresenceOfChildrenCode, fill = Education)) +
    labs(title = "Presence of Children and Education Level for Home Prices between $170,000 and $175,000", 
         x = "Presence of Children", y = "Count")
ggsave("insight_1.png")
  
############ INSIGHT 2 ################

#Understanding the values inside OccupationIndustry
table(bbyLoyalty$OccupationIndustry)

#Creating variable without the unknown or Other OccupationIndustry
known_OccupationIndustry <- subset(bbyLoyalty, !OccupationIndustry %in% c("Unknown", "Other"))
table(known_OccupationIndustry$OccupationIndustry)

#Bar graph without unknown and others
# Plotting to see the most common Known OccupationIndustry among customers
ggplot(data = known_OccupationIndustry) +
  geom_bar(aes(y = OccupationIndustry)) +
  labs(x = "Occupation Industry", y = "Frequency") +
  theme_bw()


#Bar graphs for all occupations distributions
# Plotting to see the most common Known OccupationIndustry among customers
ggplot(data = known_OccupationIndustry) +
  geom_bar(aes(x = storeVisitFrequency, fill = OccupationIndustry)) +
  labs(x = "Store Visit Frequency", y = "Frequency", fill = "Occupation Industry") +
  theme_bw()

#Selecting the Occupations
  #info on each occupation
  summaryoccupation_household <- by(bbyLoyalty$y_householdSpend, bbyLoyalty$OccupationIndustry, summary)
  summaryoccupation_household
  #The occupations with the largest range (max-min): Education ( 660 ), Medical ( 668), Financial Services  (693)
  selected_occupations <- c("Education", "Medical", "Financial Services")

  #Aggregating functions
  min_household_spend <- aggregate(y_householdSpend~OccupationIndustry, 
                        data = bbyLoyalty[bbyLoyalty$OccupationIndustry %in% selected_occupations,], FUN = min)
  max_household_spend <- aggregate(y_householdSpend~OccupationIndustry, 
                        data = bbyLoyalty[bbyLoyalty$OccupationIndustry %in% selected_occupations,], FUN = max)

  # Merging the data
  range <- merge(min_household_spend, max_household_spend, by = "OccupationIndustry")
  colnames(range) <- c("OccupationIndustry", "Min_Spend", "Max_Spend")
  
  # Creating the bar graph with error bars
  ggplot(data = range, aes(x = OccupationIndustry)) +
    geom_bar(aes(y = Min_Spend, fill = OccupationIndustry), stat = "identity", position = "dodge") +
    geom_bar(aes(y = Max_Spend, fill = OccupationIndustry), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Min_Spend, ymax = Max_Spend), width = 0.2, position = position_dodge(0.9)) +
    labs(x = "Occupation Industry", y = "Household Spend", fill = "") +
    ggtitle("Household Spend by Occupation Industry with Largest Range") +
    theme_bw() +
    geom_text(aes(label = Min_Spend, y = Min_Spend), vjust = -0.5, position = position_dodge(0.9), color = "black") +
    geom_text(aes(label = Max_Spend, y = Max_Spend), vjust = -0.5, position = position_dodge(0.9), color = "black")
  ggsave("insight_2.png")
  
############ INSIGHT 3 ################

unique(bbyLoyalty$state)

#Non US states
states_to_remove <- c('Nuevo Le\303\263n','British Columbia','Alberta','Coahuila','Tamaulipas')
# Identify rows where the state is in the states_to_remove vector
rows_to_remove <- bbyLoyalty$state %in% states_to_remove
#Cleaning dataset to remove those states
bbyLoyalty <- bbyLoyalty[!rows_to_remove, ]

#Understanding distribution of gender and state for US states
ggplot(bbyLoyalty, aes(y = state, fill = Gender)) +
    geom_bar() +
    labs(title = "Relationship between State and Gender", 
         x = "Count", y = "State") 

# Create frequency table of state variable
state_counts <- table(bbyLoyalty$state)
# Sort frequency table in descending order
state_counts_sorted <- sort(state_counts, decreasing = TRUE)
# Subset the top 5 states with counts above 350
top_5_states <- names(state_counts_sorted[state_counts_sorted > 350])[1:5]

top_5_states
  #"Texas","California","Pennsylvania", "Illinois", "New York" 

#Subsetting the graph for only the top states
bbyLoyalty_top5 <- subset(bbyLoyalty, state %in% top_5_states)
# Create bar plot for top 5 states and gender
ggplot(bbyLoyalty_top5, aes(y = state, fill = Gender)) +
  geom_bar() +
  labs(title = "Relationship between State and Gender for Top 5 States", 
       x = "Count", y = "State")

table(bbyLoyalty$y_householdSpend) 

# Calculate spend_per_visit by state and gender
spend_per_visit_by_state_gender <- bbyLoyalty_top5 %>% 
  group_by(state, Gender) %>% 
  summarize(spend_per_visit = round(sum(y_householdSpend) / sum(storeVisitFrequency)))

# Create bar plot for top 5 states, gender, and spend per visit
ggplot(spend_per_visit_by_state_gender, aes(x = state, y = spend_per_visit, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = spend_per_visit), 
            position = position_dodge(width = 1), 
            vjust = -0.5, size = 3) +
  labs(title = "Household Spend per Visit by Gender for Top 5 States",
       x = "State", y = "Household Spend per Visit")
ggsave("insight_3.png")

#Exploring the data for political description
ggplot(bbyLoyalty_top5, aes(y = state, fill = Gender)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ PartiesDescription) +
  labs(title = "Relationship between State, Gender, and Political Affiliation for Top 5 States",
       x = "Count", y = "State") +
  scale_fill_manual(values = c("#00BFFF", "#FF69B4"), name = "Gender", labels = c("Male", "Female")) +
  theme(legend.position = "bottom")

############ INSIGHT 4 ################

table(bbyLoyalty$HealthFitnessMagazineInHome)

#Distribution for Health Fitness Magazine
plot_histogram(bbyLoyalty$HealthFitnessMagazineInHome)
  #Most don't have Health Fitness Magazine

#Distribution for FamilyMagazineInHome
plot_histogram(bbyLoyalty$FamilyMagazineInHome)
  #Most don't have Family Magazine

#Distribution for BookBuyerInHome
plot_histogram(bbyLoyalty$BookBuyerInHome)
  #Most don't have BookBuyerInHome

    ### based on these- those are not good things to invest on #####

#Networth
bbyLoyalty$NetWorth

# Create a vector of categories or ranges of net worth values
net_worth_categories <- c("1-4999", "5000-9999", "10000-24999", "25000-49999", "50000-99999",'100000-249999','250000-499999','499999+')
# Create a new factor variable with the desired order of levels
net_worth_factor <- factor(bbyLoyalty$NetWorth, levels = net_worth_categories)

#Distribution for NetWorth
ggplot(bbyLoyalty, aes(x = net_worth_factor)) +
  geom_bar(fill = "steelblue")+
  labs(title = "Distribution of Net Worth", x = "Net Worth Categories") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 09))
ggsave("distribution_net_worth.png")

##Getting x axis as Net Worth and property type as stacked variables
ggplot(bbyLoyalty, aes(y = net_worth_factor, fill = PropertyType)) +
  geom_bar() +
  labs(title = "Relationship between Networth and Property Type", 
       y = "Net Worth", x = "Count")

##Getting x axis as Net Worth and state as stacked variables
ggplot(bbyLoyalty, aes(y = net_worth_factor, fill = state)) +
  geom_bar() +
  labs(title = "Distribution of Networth Across States", 
       y = "Net Worth", x = "Count")
ggsave("distribution_net_worth_state.png")

##Getting x axis as Net Worth and EthnicDescription as stacked variables
ggplot(bbyLoyalty, aes(y = net_worth_factor, fill = EthnicDescription)) +
  geom_bar() +
  labs(title = "Distribution of Networth by EthnicDescription", 
       y = "Net Worth", x = "Count")

##Getting x axis as Net Worth and ResidenceHHGenderDescription as stacked variables
ggplot(bbyLoyalty, aes(y = net_worth_factor, fill = ResidenceHHGenderDescription)) +
  geom_bar() +
  labs(title = "Distribution of Networth by Head of household gender", 
       y = "Net Worth", x = "Count")
ggsave("insight_4.png")

summary(bbyLoyalty$Age)

#Age Stats
ggplot(bbyLoyalty, aes(y = "", x = Age)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  stat_summary(fun = min, geom = "text", aes(label = paste0("Min: ", min(Age))),
               vjust = 1.5, size = 3, color = "black") +
  stat_summary(fun = max, geom = "text", aes(label = paste0("Max: ", max(Age))),
               vjust = -1.5, size = 3, color = "black") +
  stat_summary(geom = "errorbar", fun.data = "median_hilow", width = 0.5, 
               color = "red", size = .8) +
  labs(title = "Boxplot of Age Distribution",
       x = "Age", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_blank())
ggsave("age_distribution.png")


## Save a copy of the joined and cleaned data with the following code.  It will save to your local working directory.
write.csv(bbyLoyalty, 'bbyLoyalty.csv', row.names = F)


#End
