## NGC-6543
## 2020-11-04

###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-0 Load Energy
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################

# File saved with UTF-8 encoding

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(dplyr)) install.packages('dplyr')

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()



# Kaggle URL where all_energy_statistics.csv is available: 
# https://www.kaggle.com/unitednations/international-energy-statistics


FileName <- './source_data/all_energy_statistics.csv'

energy <- read.csv(FileName
                , quote = '"'
                , sep = ","
                , header = TRUE
                , stringsAsFactors = TRUE
                , encoding ='UTF-8'
                )

energy <- energy[, -which( names(energy) == 'quantity_footnotes') ]
names(energy)[c(1,2)] <- c('country', 'transaction')

# Special characters, unicode used for them.
# There are some En dashes where there should be hyphens.
# There are no Em dashes.
# Hyphen
# -
# En dash:
# –
# Em dash:
# — 

# CHECK HOW STRINGS WERE ENCODED
unique(energy[grep('.*–.*',energy$transaction, ignore.case = T), 'transaction']) # en-dashes
unique(energy[grep('.*—.*',energy$transaction, ignore.case = T), 'transaction']) # em-dashes

energy$transaction <- gsub("–", "-", energy$transaction) # replace en-dashes (UTF-8)!
energy$category <- gsub("_"," ", energy$category) # remove underscore from category names

###################################################################################################
# Keep certain categories, and remove certain categoies for the analysis - see notes.

energy <- energy[which( grepl('.*imports$|.*exports$|.*production$|.*final energy consumption$'
                              ,energy$transaction
                              , ignore.case = T) &
  
                        ! grepl('.*gross production.*'
                            ,energy$transaction
                            , ignore.case = T) &
                         
                        ! grepl('.*used for electricity production.*'
                            ,energy$transaction
                            , ignore.case = T) ) , ]

###################################################################################################
# Simplify transactions to one of four types (import, export, production, consumption)

simplify <- function(t) {
  tolower(
    substr(t
           ,nchar(t) - regexpr(' ', intToUtf8(rev(utf8ToInt(t))) )[1] + 2
           ,nchar(t)
    )
  )
}

energy$transaction <- apply(energy, 1, function(x) simplify(x[2]) )





###################################################################################################
# Join to energy_codes file which replaces individual categories with parent categories.
# This was necessary to generalize the analysis to broad categories like 'oil', 'gas', 'primary renewables' etc.


FileName <- './source_data/energy_codes.csv'

# load file category codes and conversion factors
energy_codes <- read.csv(FileName
                   , quote = '"'
                   , sep = ","
                   , header = TRUE
                   , stringsAsFactors = TRUE
                   , encoding ='UTF-8')


energy$transaction <- factor(energy$transaction)
energy$category <- factor(energy$category)

energy <- merge(
  x = energy
  ,y = energy_codes
  ,by.x = 'category'
  ,by.y = 'category'
  ,all.x = T)

rm(energy_codes)

energy$quantity <- energy$quantity * energy$TJ_conversion
energy <- energy[,which(names(energy) %in% 
                          c('country'
                            ,'year'
                            ,'quantity'
                            ,'category'
                            ,'transaction'
                            ,'product'
                            ,'category_code'
                            ,'type'
                            ,'subtype'
                            ) ) ]

# # energy <- energy[ order(energy$country, energy$year, energy$transaction, energy$type, energy$subtype), ]
# energy <- energy[ with(energy, order(country, year, transaction, type, subtype)), ]

# Check for duplicates
# nrow(unique(energy))
# nrow(energy)

# sum the quantities for the simplified categories
energy <- aggregate(quantity ~ country + year + transaction + product + type + subtype
                    , energy
                    , sum)

# rearrange columns
energy <- data.frame(country = energy$country
                     , year = energy$year
                     , quantity = energy$quantity
                     , transaction = energy$transaction
                     , product = energy$product
                     , type = energy$type
                     , subtype = energy$subtype)

# sort the columns
energy <- energy[ with(energy, order(country, year, transaction, type, subtype)), ]

# convert to country column to character in order to complete the next steps
energy$country <- as.character(energy$country)

# energy[grep('.*Ivoire$',energy$country, ignore.case = T), 'country']

# update country names to those that will be recognized by the r countrycode package
energy[energy$country =="Antigua and Barbuda" , 'country'] <- "Antigua & Barbuda"
energy[energy$country =="Bolivia (Plur. State of)" , 'country'] <- "Bolivia"
energy[energy$country =="Bosnia and Herzegovina" , 'country'] <- "Bosnia & Herzegovina"
energy[energy$country =="Brunei Darussalam" , 'country'] <- "Brunei"
energy[energy$country =="Cabo Verde" , 'country'] <- "Cape Verde"
energy[energy$country =="Central African Rep." , 'country'] <- "Central African Republic"
energy[energy$country =="China, Hong Kong SAR" , 'country'] <- "Hong Kong SAR China"
energy[energy$country =="China, Macao SAR" , 'country'] <- "Macau SAR China"
energy[energy$country =="Congo" , 'country'] <- "Congo - Brazzaville"
energy[energy$country =="Dem. Rep. of the Congo" , 'country'] <- "Congo - Kinshasa"
energy[energy$country =="Ethiopia, incl. Eritrea" , 'country'] <- "Ethiopia"
energy[energy$country =="Faeroe Islands" , 'country'] <- "Faroe Islands"
energy[energy$country =="Falkland Is. (Malvinas)" , 'country'] <- "Falkland Islands"
energy[energy$country =="Iran (Islamic Rep. of)" , 'country'] <- "Iran"
energy[energy$country =="Korea, Dem.Ppl's.Rep." , 'country'] <- "North Korea"
energy[energy$country =="Korea, Republic of" , 'country'] <- "South Korea"
energy[energy$country =="Lao People's Dem. Rep." , 'country'] <- "Laos"
energy[energy$country =="Micronesia (Fed. States of)" , 'country'] <- "Micronesia (Federated States of)"
energy[energy$country =="Myanmar" , 'country'] <- "Myanmar (Burma)"
energy[energy$country =="Republic of Moldova" , 'country'] <- "Moldova"
energy[energy$country =="Russian Federation" , 'country'] <- "Russia"
energy[energy$country =="Sao Tome and Principe" , 'country'] <- "São Tomé & Príncipe" # non-UTF-8!
energy[energy$country =="Serbia and Montenegro" , 'country'] <- "Serbia"
energy[energy$country =="Sint Maarten (Dutch part)" , 'country'] <- "Sint Maarten"
energy[energy$country =="St. Helena and Depend." , 'country'] <- "St. Helena"
energy[energy$country =="St. Kitts-Nevis" , 'country'] <- "St. Kitts & Nevis"
energy[energy$country =="St. Pierre-Miquelon" , 'country'] <- "St. Pierre & Miquelon"
energy[energy$country =="St. Vincent-Grenadines" , 'country'] <- "St. Vincent & Grenadines"
energy[energy$country =="State of Palestine" , 'country'] <- "Palestinian Territories"
energy[energy$country =="Syrian Arab Republic" , 'country'] <- "Syria"
energy[energy$country =="Trinidad and Tobago" , 'country'] <- "Trinidad & Tobago"
energy[energy$country =="Turks and Caicos Islands" , 'country'] <- "Turks & Caicos Islands"
energy[energy$country =="United Rep. of Tanzania" , 'country'] <- "Tanzania"
energy[energy$country =="United States Virgin Is." , 'country'] <- "U.S. Virgin Islands"
energy[energy$country =="Venezuela (Bolivar. Rep.)" , 'country'] <- "Venezuela"
energy[energy$country =="Viet Nam" , 'country'] <- "Vietnam"
energy[energy$country =="Wallis and Futuna Is." , 'country'] <- "Wallis & Futuna"
energy[energy$country =="German Dem. R. (former)" , 'country'] <- "Germany"
energy[energy$country =="Germany, Fed. R. (former)" , 'country'] <- "Germany"
energy[energy$country =="Czechoslovakia (former)" , 'country'] <- "Czechia"
energy[energy$country =="Sudan (former)" , 'country'] <- "Sudan"
energy[energy$country =="T.F.Yug.Rep. Macedonia" , 'country'] <- "Macedonia"
energy[energy$country =="USSR (former)" , 'country'] <- "Russia"
energy[energy$country =="Yemen, Dem. (former)" , 'country'] <- "Yemen"

# drop any country names that are not in the r countrycode package
energy <- energy[energy$country != 'Antarctic Fisheries',]
energy <- energy[energy$country != 'Bonaire, St Eustatius, Saba',]
energy <- energy[energy$country != 'Commonwealth of Independent States (CIS)',]
energy <- energy[energy$country != 'Neth. Antilles (former)',]
energy <- energy[energy$country != 'Other Asia',]
energy <- energy[energy$country != 'Pacific Islands (former)',]
energy <- energy[energy$country != 'Yemen Arab Rep. (former)',]
energy <- energy[energy$country != 'Yugoslavia, SFR (former)',]


# unique(Encoding(energy$country))
# energy$country[Encoding(energy$country)=='latin1']

# The above string subtitutions may have encorporated non-UTF-8 encoded characters; convert to UTF-8.
energy$country <- enc2utf8(energy$country)
# convert country back to a factor
energy$country <- factor(energy$country)



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-0 Load HDI
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



# File saved with UTF-8 encoding

library(dplyr)
require(tidyverse)

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()


FileName <- './source_data/Human development index (HDI).csv'

hdi <- read.csv(FileName
                , quote = '"'
                , sep = ","
                , header = TRUE
                , stringsAsFactors = TRUE
                , na.strings = '..'
                , skip = 1)

hdi <- hdi[,-1]
hdi <- gather(hdi, year, hdi_value, -Country)
hdi$year <- gsub("X","", hdi$year)
hdi$year <- as.integer(hdi$year)
names(hdi)[1]<-'country'

hdi$country <- as.character(hdi$country)

hdi[hdi$country =="Antigua and Barbuda", 'country'] <- "Antigua & Barbuda"
hdi[hdi$country =="Bolivia (Plurinational State of)", 'country'] <- "Bolivia"
hdi[hdi$country =="Bosnia and Herzegovina", 'country'] <- "Bosnia & Herzegovina"
hdi[hdi$country =="Brunei Darussalam", 'country'] <- "Brunei"
hdi[hdi$country =="Cabo Verde", 'country'] <- "Cape Verde"
hdi[hdi$country =="Congo", 'country'] <- "Congo - Brazzaville"
hdi[hdi$country =="Congo (Democratic Republic of the)", 'country'] <- "Congo - Kinshasa"
hdi[hdi$country =="Hong Kong, China (SAR)", 'country'] <- "Hong Kong SAR China"
hdi[hdi$country =="Iran (Islamic Republic of)", 'country'] <- "Iran"
hdi[hdi$country =="Korea (Republic of)", 'country'] <- "South Korea"
hdi[hdi$country =="Lao People's Democratic Republic", 'country'] <- "Laos"
hdi[hdi$country =="The former Yugoslav Republic of Macedonia", 'country'] <- "Macedonia"
hdi[hdi$country =="Moldova (Republic of)", 'country'] <- "Moldova"
hdi[hdi$country =="Myanmar", 'country'] <- "Myanmar (Burma)"
hdi[hdi$country =="Palestine, State of", 'country'] <- "Palestinian Territories"
hdi[hdi$country =="Russian Federation", 'country'] <- "Russia"
hdi[hdi$country =="Sao Tome and Principe", 'country'] <- "São Tomé & Príncipe"
hdi[hdi$country =="Saint Kitts and Nevis", 'country'] <- "St. Kitts & Nevis"
hdi[hdi$country =="Saint Lucia", 'country'] <- "St. Lucia"
hdi[hdi$country =="Saint Vincent and the Grenadines", 'country'] <- "St. Vincent & Grenadines"
hdi[hdi$country =="Syrian Arab Republic", 'country'] <- "Syria"
hdi[hdi$country =="Tanzania (United Republic of)", 'country'] <- "Tanzania"
hdi[hdi$country =="Trinidad and Tobago", 'country'] <- "Trinidad & Tobago"
hdi[hdi$country =="Venezuela (Bolivarian Republic of)", 'country'] <- "Venezuela"
hdi[hdi$country =="Viet Nam", 'country'] <- "Vietnam"





# unique(Encoding(hdi$country))
# hdi$country[Encoding(hdi$country)=='latin1']
# hdi$country[Encoding(hdi$country)=='UTF-8']

hdi$country <- enc2utf8(hdi$country)
# Encoding(hdi$country)



hdi <- unique(hdi)

hdi$country <- factor(hdi$country)
hdi$year <- as.integer(hdi$year)

hdi<- hdi[order(hdi$country, hdi$year),]



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-0 Load Population
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



# File saved with UTF-8 encoding

library(dplyr)

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()


FileName <- './source_data/UNdata_Export_20190814_015840746.csv'

pop <- read.csv(FileName
                , quote = '"'
                , sep = ","
                , header = TRUE
                , stringsAsFactors = TRUE
                , encoding ='UTF-8')

# str(pop)
# summary(pop)

pop <- pop %>% 
  filter(Area=='Total') %>%
  filter(Sex=='Both Sexes') %>%
  group_by(Country.or.Area, Year) %>%
  summarise(Value=mean(Value))

pop$Value <- floor(pop$Value)

pop <- as.data.frame(pop)

names(pop) <- c('country','year','population')

pop$country <- as.character(pop$country)

pop[pop$country =="Antigua and Barbuda", 'country'] <- "Antigua & Barbuda"
pop[pop$country =="Bolivia (Plurinational State of)", 'country'] <- "Bolivia"
pop[pop$country =="Bosnia and Herzegovina", 'country'] <- "Bosnia & Herzegovina"
pop[pop$country =="Brunei Darussalam", 'country'] <- "Brunei"
pop[pop$country =="Cabo Verde", 'country'] <- "Cape Verde"
pop[pop$country =="Congo", 'country'] <- "Congo - Brazzaville"
pop[pop$country =="Democratic Republic of the Congo", 'country'] <- "Congo - Kinshasa"
pop[pop$country =="Falkland Islands (Malvinas)", 'country'] <- "Falkland Islands"
pop[pop$country =="Faeroe Islands", 'country'] <- "Faroe Islands"
pop[pop$country =="China, Hong Kong SAR", 'country'] <- "Hong Kong SAR China"
pop[pop$country =="Iran (Islamic Republic of)", 'country'] <- "Iran"
pop[pop$country =="Lao People's Democratic Republic", 'country'] <- "Laos"
pop[pop$country =="China, Macao SAR", 'country'] <- "Macau SAR China"
pop[pop$country =="TFYR of Macedonia", 'country'] <- "Macedonia"
pop[pop$country =="Republic of Moldova", 'country'] <- "Moldova"
pop[pop$country =="Myanmar", 'country'] <- "Myanmar (Burma)"
pop[pop$country =="Democratic People's Republic of Korea", 'country'] <- "North Korea"
pop[pop$country =="State of Palestine", 'country'] <- "Palestinian Territories"
pop[pop$country =="Reunion", 'country'] <- "Réunion"
pop[pop$country =="Russian Federation", 'country'] <- "Russia"
pop[pop$country =="Sao Tome and Principe", 'country'] <- "São Tomé & Príncipe"
pop[pop$country =="Sint Maarten (Dutch part)", 'country'] <- "Sint Maarten"
pop[pop$country =="Republic of Korea", 'country'] <- "South Korea"
pop[pop$country =="Republic of South Sudan", 'country'] <- "South Sudan"
pop[pop$country =="Saint Helena ex. dep.", 'country'] <- "St. Helena"
pop[pop$country =="Saint Kitts and Nevis", 'country'] <- "St. Kitts & Nevis"
pop[pop$country =="Saint Lucia", 'country'] <- "St. Lucia"
pop[pop$country =="Saint Pierre and Miquelon", 'country'] <- "St. Pierre & Miquelon"
pop[pop$country =="Saint Vincent and the Grenadines", 'country'] <- "St. Vincent & Grenadines"
pop[pop$country =="Syrian Arab Republic", 'country'] <- "Syria"
pop[pop$country =="United Republic of Tanzania", 'country'] <- "Tanzania"
pop[pop$country =="Trinidad and Tobago", 'country'] <- "Trinidad & Tobago"
pop[pop$country =="Turks and Caicos Islands", 'country'] <- "Turks & Caicos Islands"
pop[pop$country =="United Kingdom of Great Britain and Northern Ireland", 'country'] <- "United Kingdom"
pop[pop$country =="United States Virgin Islands", 'country'] <- "U.S. Virgin Islands"
pop[pop$country =="United States of America", 'country'] <- "United States"
pop[pop$country =="Venezuela (Bolivarian Republic of)", 'country'] <- "Venezuela"
pop[pop$country =="Viet Nam", 'country'] <- "Vietnam"
pop[pop$country =="Wallis and Futuna Islands", 'country'] <- "Wallis & Futuna"



pop <- pop[pop$country != 'Saint-Barthélemy',]
pop <- pop[pop$country != 'Åland Islands',]
pop <- pop[pop$country != 'Eswatini',]
pop <- pop[pop$country != 'Holy See',]
pop <- pop[pop$country != 'Monaco',]
pop <- pop[pop$country != 'Norfolk Island',]
pop <- pop[pop$country != 'Pitcairn',]
pop <- pop[pop$country != 'Saint Helena: Ascension',]
pop <- pop[pop$country != 'Saint Helena: Tristan da Cunha',]
pop <- pop[pop$country != 'Saint-Martin (French part)',]
pop <- pop[pop$country != 'San Marino',]
pop <- pop[pop$country != 'Svalbard and Jan Mayen Islands',]
pop <- pop[pop$country != 'Tokelau',]
pop <- pop[pop$country != 'Western Sahara',]



rownames(pop) <- NULL

pop <- unique(pop)


# The above string subtitutions may have encorporated non-UTF-8 encoded characters; convert to UTF-8.
# unique(Encoding(pop$country))
# pop$country[Encoding(pop$country)=='latin1']
# pop$country[Encoding(pop$country)=='UTF-8']
pop$country <- enc2utf8(pop$country)
pop$country <- factor(pop$country)
pop$year <- as.integer(pop$year)



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-1 load and clean data
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



if (!require(tidyverse)) {install.packages('tidyverse') 
  require(tidyverse) }

if (!require(countrycode)) install.packages('countrycode')
if (!require(mapproj)) install.packages('mapproj')
if (!require(maps)) install.packages('maps')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(RColorBrewer)) install.packages('RColorBrewer')


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()



# *********** Note ************ 
# The following scripts should have been run before running this script:
# source('data-0 load energy.r')
# source('data-0 load population.r')
# source('data-0 load hdi.r')



# options(scipen=999)
# options(scipen=7)
# reduces the size of the numeric labels in charts. We might consider rescaling instead.
options(scipen=-1) 

# themes:
theme_set(theme_gray())  


# this file was created in internationalEnergy_odbc_load.R - why don't we just load it as UTF-8?
FileName <- './source_data/countryCodes.csv'

countryCodes <- read.csv(FileName
                         , quote = '"'
                         , sep = ","
                         , header = TRUE
                         , stringsAsFactors = F
)

countryCodes <- countryCodes[,-1]

countryCodes$country <- enc2utf8(countryCodes$country)
countryCodes$iso2c <- enc2utf8(countryCodes$iso2c)
countryCodes$iso3c <- enc2utf8(countryCodes$iso3c)
countryCodes$continent <- enc2utf8(countryCodes$continent)
countryCodes$region <- enc2utf8(countryCodes$region)
countryCodes$map_country <- enc2utf8(countryCodes$map_country)

# unique(Encoding(countryCodes$country))
# countryCodes$country[Encoding(countryCodes$country)=='UTF-8']
# unique(Encoding(countryCodes$iso2c))
# unique(Encoding(countryCodes$iso3c))
# unique(Encoding(countryCodes$continent))
# unique(Encoding(countryCodes$region))
# unique(Encoding(countryCodes$map_country))

countryCodes$country <- factor(countryCodes$country)
countryCodes$iso2c <- factor(countryCodes$iso2c)
countryCodes$iso3c <- factor(countryCodes$iso3c)
countryCodes$continent <- factor(countryCodes$continent)
countryCodes$region <- factor(countryCodes$region)
countryCodes$map_country <- factor(countryCodes$map_country)

# merge the countryCodes to the energy file to get the map names
energy <- merge(energy,countryCodes, by.x='country', by.y='country', all.x = TRUE)

# levels(energy$subtype)
# levels(energy$transaction)

######################################################################################################
# Replace missing values in pop and hdi and eliminate those with too many missing values

# 1 - keep only years from 1990 to 2014
# 2 - determine the number of missing years for each country 
# 3 - drop any country with NAs in greater than 50% of the years for a given country
# 4 - and then for each remaining missing value:
# A if the NA is in the bottom 5 it is replaced with the min value for the country
# B if the NA is in the top 5 it is replaced with the max value for the country
# C if the NA is in the middle 15 it is replaced with the mean value for the country

# str(pop)
pop <- pop[pop$year >= 1990 & pop$year < 2015,]
hdi <- hdi[hdi$year >= 1990 & hdi$year < 2015,]
# head(pop)
rownames(pop) <- NULL
rownames(hdi) <- NULL


# Function to replace missing values
na.replace <- function(target.df, replace.vect, start.idx, end.idx)
{
  for (j in 1:nrow(target.df) ) {
    for (i in start.idx:end.idx){
      
      if ( is.na(target.df[j,i]) ) {
        target.df[j,i] <- replace.vect[j]
      } 
      else {}
    }
  }
  return(target.df)
}


#######################################################################################
# Replace missing values in pop  

df <- pop
names(df) <- c('country','year','value')

df.wide <- spread(df,year,value)
# head(df.wide,15)
# dim(df.wide)
# sum(is.na(df.wide))
df.wide.na <- apply(df.wide[,-1], 2, function(x) is.na(x) )
# head(df.wide.na)
df.wide.sum <- apply(df.wide.na, 1, function(x) sum(x))
# head(df.wide.sum)
# str(df.wide.sum)

# df.wide <- cbind(df.wide, df.wide.sum)    
# df.wide[order(-df.wide$df.wide.sum) , c('country','df.wide.sum')]

df.len<- ncol(df.wide)
df.mid <- floor(ncol(df.wide)/2)
df.qtr1 <- floor(ncol(df.wide)/4)
df.qtr4 <- ( ncol(df.wide) - floor(ncol(df.wide)/4) )+1

# drop any columns with greater than ~ 50% NAs

df.wide <- df.wide[df.wide.sum < df.mid, ]

# impute values for the rest of the NAs based on where they are in the timeline in years

df.wide.min <- apply(df.wide[,2:df.len], 1, function(x) min(x, na.rm = TRUE) )
df.wide.mean <- apply(df.wide[,2:df.len], 1, function(x) mean(x, na.rm = TRUE) )
df.wide.max <- apply(df.wide[,2:df.len], 1, function(x) max(x, na.rm = TRUE) )

df.wide <- na.replace(df.wide, df.wide.min, 2, df.qtr1)
df.wide <- na.replace(df.wide, df.wide.mean, df.qtr1+1, df.qtr4)
df.wide <- na.replace(df.wide, df.wide.max, df.qtr4+1, df.len)


df.long <- gather(df.wide, year, value, -country)
# head(df.long)
# tail(df.long)

df <- df.long

rm(df.long, df.wide, df.wide.na, df.wide.sum, df.wide.min, df.wide.max, df.wide.mean)
rm(df.len, df.mid, df.qtr1, df.qtr4)

pop <- df
rm(df)
names(pop)[3] <- 'population'


#######################################################################################
# Replace missing values in hdi  

df <- hdi
names(df) <- c('country','year','value')

df.wide <- spread(df,year,value)
# head(df.wide,15)
# dim(df.wide)
# sum(is.na(df.wide))
df.wide.na <- apply(df.wide[,-1], 2, function(x) is.na(x) )
# head(df.wide.na)
df.wide.sum <- apply(df.wide.na, 1, function(x) sum(x))
# head(df.wide.sum)
# str(df.wide.sum)

# df.wide <- cbind(df.wide, df.wide.sum)    
# df.wide[order(-df.wide$df.wide.sum) , c('country','df.wide.sum')]

df.len<- ncol(df.wide)
df.mid <- floor(ncol(df.wide)/2)
df.qtr1 <- floor(ncol(df.wide)/4)
df.qtr4 <- ( ncol(df.wide) - floor(ncol(df.wide)/4) )+1

# drop any columns with greater than ~ 50% NAs

df.wide <- df.wide[df.wide.sum < df.mid, ]

# impute values for the rest of the NAs based on where they are in the timeline in years

df.wide.min <- apply(df.wide[,2:df.len], 1, function(x) min(x, na.rm = TRUE) )
df.wide.mean <- apply(df.wide[,2:df.len], 1, function(x) mean(x, na.rm = TRUE) )
df.wide.max <- apply(df.wide[,2:df.len], 1, function(x) max(x, na.rm = TRUE) )

df.wide <- na.replace(df.wide, df.wide.min, 2, df.qtr1)
df.wide <- na.replace(df.wide, df.wide.mean, df.qtr1+1, df.qtr4)
df.wide <- na.replace(df.wide, df.wide.max, df.qtr4+1, df.len)


df.long <- gather(df.wide, year, value, -country)
# head(df.long)
# tail(df.long)

df <- df.long

rm(df.long, df.wide, df.wide.na, df.wide.sum, df.wide.min, df.wide.max, df.wide.mean)
rm(df.len, df.mid, df.qtr1, df.qtr4)

hdi <- df
rm(df)
names(hdi)[3] <- 'hdi'


#######################################################################################
# Append the energy and population dfs using the country names to join to energy df

energy <- merge(
  x = energy
  ,y = pop
  ,by = c("country","year")
  ,all.x = TRUE)

energy <- merge(
  x = energy
  ,y = hdi
  ,by = c("country","year")
  ,all.x = TRUE)

# head(energy)

rm(pop, hdi, countryCodes)


#######################################################################################
# Create a calculated column for quantity per capita
energy$percapita <- energy$quantity / energy$population



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-2 build transactions
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



# Build the dataframes for consumption, production, imports, exports

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()


# *********** Note ************ 
# The following scripts should have been run before running this script:
# source('data-1 load and clean data.r')


# Consumption #######################################################################################
# Consumption1 df for oil, coal, bio, gas.
# Consumption2 df is actually production for thermal, nuclear, primary renewables
# since any production of electricity and heat are assumed to be ~100% consumed.

consumption1 <- energy[ energy$type %in% c('Biofuels and Waste'
                                           ,'Coal'
                                           ,'Oil'
                                           ,'Natural Gas') &
                          energy$transaction=='consumption' &
                          (energy$year >= 1990 & energy$year < 2015)
                        , ]

consumption2 <- energy[ energy$type %in% c('Thermal Electricity'
                                           ,'Nuclear Electricity'
                                           ,'Primary Electricity, Renewable') &
                          energy$transaction=='production' &
                          (energy$year >= 1990 & energy$year < 2015)
                        , ]

consumption <- rbind(consumption1,consumption2)
rm(consumption1, consumption2)


# Production #######################################################################################
# production1 df for oil, coal, bio, gas.
# production2 df is actually production for thermal, nuclear, primary renewables


production1 <- energy[ energy$type %in% c('Biofuels and Waste'
                                          ,'Coal'
                                          ,'Oil'
                                          ,'Natural Gas') &
                         energy$transaction=='production' &
                         (energy$year >= 1990 & energy$year < 2015)
                       , ]

production2 <- energy[ energy$type %in% c('Thermal Electricity'
                                          ,'Nuclear Electricity'
                                          ,'Primary Electricity, Renewable') &
                         energy$transaction=='production' &
                         (energy$year >= 1990 & energy$year < 2015)
                       , ]

production <- rbind(production1,production2)
rm(production1, production2)



# Exports #######################################################################################

# Note there is no import/export of types and subtypes
# under products of "Electricity production processes"
# so products of "Electricity and Heat" are used instead.

# table(energy$transaction, energy$product)
# table(energy$transaction, energy$type)
# table(energy$transaction, energy$subtype)


exports1 <- energy[ energy$type %in% c('Biofuels and Waste'
                                       ,'Coal'
                                       ,'Oil'
                                       ,'Natural Gas'
                                       ,'Electricity and Heat') &
                      energy$transaction=='exports' &
                      (energy$year >= 1990 & energy$year < 2015)
                    , ]


exports <- rbind(exports1)
rm(exports1)


# Imports #######################################################################################


# Note there is no import/export of types and subtypes
# under products of "Electricity production processes"
# so products of "Electricity and Heat" are used instead.

# table(energy$transaction, energy$product)
# table(energy$transaction, energy$type)
# table(energy$transaction, energy$subtype)


imports1 <- energy[ energy$type %in% c('Biofuels and Waste'
                                       ,'Coal'
                                       ,'Oil'
                                       ,'Natural Gas'
                                       ,'Electricity and Heat') &
                      energy$transaction=='imports' &
                      (energy$year >= 1990 & energy$year < 2015)
                    , ]


imports <- rbind(imports1)
rm(imports1)


########################################################################################
# Add the calculated columns for each transaction data frame

calculate_cols <-  function(t) {
  
  sum_y <-aggregate(x=t$quantity 
                    , by=list(country = t$country, year = t$year)
                    , FUN = sum)
  
  sum_25y <-aggregate(x=t$quantity
                      , by=list(country = t$country)
                      , FUN = sum)
  
  subtype_sum_25y <-aggregate(quantity ~ country + subtype, t, sum)
  
  percapita_sum_y <- aggregate(percapita ~ country + year, t, sum)
  
  percapita_sum_25y <- aggregate(percapita ~ country, t, sum)
  
  percapita_subtype_sum_25y <- aggregate(percapita ~ country + subtype, t, sum)
  
  names(sum_y)[3]<-'sum_y'
  names(sum_25y)[2]<-'sum_25y'
  names(subtype_sum_25y)[3]<-'subtype_sum_25y'
  names(percapita_sum_y)[3]<-'percapita_sum_y'
  names(percapita_sum_25y)[2]<-'percapita_sum_25y'
  names(percapita_subtype_sum_25y)[3]<-'percapita_subtype_sum_25y'
  
  t <- merge(
    x = t
    ,y = sum_y
    ,by = c("country","year")
    ,all.x = TRUE)
  
  t <- merge(
    x = t
    ,y = sum_25y
    ,by.x = 'country'
    ,by.y = 'country'
    ,all.x = TRUE)
  
  t <- merge(
    x = t
    ,y = subtype_sum_25y
    ,by = c("country","subtype")
    ,all.x = TRUE)
  
  t <- merge(
    x = t
    ,y = percapita_sum_y
    ,by = c("country","year")
    ,all.x = TRUE)
  
  t <- merge(
    x = t
    ,y = percapita_sum_25y
    ,by = c("country")
    ,all.x = TRUE)
  
  t <- merge(
    x = t
    ,y = percapita_subtype_sum_25y
    ,by = c("country","subtype")
    ,all.x = TRUE)
  
  # This orders the data frame by sum_25y (descending) and year (descending):
  t <- t[order( -t[,'sum_25y'], -t[,'year'] ) , ]
  # t <- t[ with(t, order ('ten_year_sum', 'year') ) , ]
  rownames(t) <- NULL
  
  
  return(t)
  
}

########################################################################################
# End calculated_cols


transactions <- list(consumption, production, exports, imports)

for (i in 1:4) {
  transactions[[i]] <- calculate_cols(transactions[[i]])
}
rm(i)

consumption <- transactions[[1]]
production <- transactions[[2]]
exports <- transactions[[3]]
imports <- transactions[[4]]

rm(transactions)




# calculated columns filtered for renewables only - only for production dataframe
# (kick-out Wave subtype since it is such a tiny category and adds an 
# unnecessary group to graphics)


t <- production

t1 <- filter(t,type %in% c('Primary Electricity, Renewable') )
t1 <- filter(t1,subtype != 'Wave')

sum_y_re <- aggregate(quantity ~ country + year, t1, sum)
sum_25y_re <- aggregate(quantity ~ country, t1, sum)
percapita_sum_y_re <- aggregate(percapita ~ country + year, t1, sum)
percapita_sum_25y_re <- aggregate(percapita ~ country, t1, sum)  

names(sum_y_re)[3]<-'sum_y_re'
names(sum_25y_re)[2]<-'sum_25y_re'  
names(percapita_sum_y_re)[3]<-'percapita_sum_y_re'  
names(percapita_sum_25y_re)[2]<-'percapita_sum_25y_re'

t <- merge(
  x = t
  ,y = sum_y_re
  ,by = c("country","year")
  ,all.x = TRUE)

t <- merge(
  x = t
  ,y = sum_25y_re
  ,by.x = 'country'
  ,by.y = 'country'
  ,all.x = TRUE)

t <- merge(
  x = t
  ,y = percapita_sum_y_re
  ,by = c("country","year")
  ,all.x = TRUE)

t <- merge(
  x = t
  ,y = percapita_sum_25y_re
  ,by = c("country")
  ,all.x = TRUE)


t <- t[order( -t[,'sum_25y'], -t[,'year'] ) , ]

production <- t
rm(t, t1, sum_y_re, sum_25y_re, percapita_sum_y_re, percapita_sum_25y_re)


#######################################################################################

# Data frame for renewables only energy production.
# For our purpose this will be the same as renewable only energy consumption. 

# (kick-out Wave since it is such a tiny category and adds an unnecessary group to graphics)
production_re <- production[production$type %in% c('Primary Electricity, Renewable') &
                              production$subtype != 'Wave', ]

# order by sum of 25y (renewable only)
production_re <- production_re[order( -production_re[,'sum_25y_re'], -production_re[,'year'] ) , ]
rownames(production_re) <- NULL




###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  data-3 top-10 countries
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# *********** Note ************ 
# The following scripts should have been run before running this script:
# source('data-2 build transactions.r')


# The countries are already ordered by -(sum_25y) and -(year) so just take the top 10

top10_consumption <- consumption[consumption$country %in% unique(consumption[,1])[1:10], ]
top10_production <- production[production$country %in% unique(production[,1])[1:10], ]
top10_exports <- exports[exports$country %in% unique(exports[,1])[1:10], ]
top10_imports <- imports[imports$country %in% unique(imports[,1])[1:10], ]

top5_re <- production_re[production_re$country %in% unique(production_re[,1])[1:5], ]
top10_re <- production_re[production_re$country %in% unique(production_re[,1])[1:10], ]
top20_re <- production_re[production_re$country %in% unique(production_re[,1])[1:20], ]



################################################################################################
# Explore the data

# Who are top 10 renewable energy producers (all renewables)?
x <- aggregate(quantity ~ country,top10_re,sum)
x <- x[order( -x[,'quantity'] ) , ]
rownames(x) <- NULL
top10_sum1 <- x
rm(x)
# top10_sum1

x <- aggregate(population ~ country + year,top5_re,sum)
x <- x[order( -x[,'population'] ) , ]
rownames(x) <- NULL
top5_pop <- x
rm(x)
filter(top5_pop, year==2014)

x <- aggregate(hdi ~ country + year,top5_re,sum)
x <- x[order( -x[,'hdi'] ) , ]
rownames(x) <- NULL
top5_hdi <- x
rm(x)
filter(top5_hdi, year==2014)

# Who are top 20 renewable energy producers (all renewables)?
x <- aggregate(quantity ~ country,top20_re,sum)
x <- x[order( -x[,'quantity'] ) , ]
rownames(x) <- NULL
top20_sum1 <- x
rm(x)
# top20_sum1


# Generate Subtotals for the top 5

# by country
x <- aggregate(quantity ~ country,top5_re,sum)
x <- x[ order(-x$quantity), ]
rownames(x) <- NULL
top5_sum1 <- x
rm(x)
# top5_sum1

# by country, subtype
x <- aggregate(quantity ~ country + subtype,top5_re,sum)
x <- x[order(x$country, x$subtype), ]
rownames(x) <- NULL
top5_sum2 <- x
rm(x)
# top5_sum2

# by country, subtype, year
x <- aggregate(quantity ~ country + subtype + year,top5_re,sum)
x <- x[order(x$country, x$subtype, x$year), ]
rownames(x) <- NULL
top5_sum3 <- x
rm(x)
# top5_sum3


# Who are the 10 ten renewable energy producers (by renewable subtype)?
top10_bysubtype <- function(df, st) {
  df.st <- df[df$subtype==st, ]
  df.st <- aggregate(quantity ~ country, df.st, sum)
  df.st <- df.st[order( -df.st[,'quantity'] ) , ]
  df.st <- df.st[1:10,]
  rownames(df.st) <- NULL
  return(df.st)
}

# top10_bysubtype(production,'Solar')
# top10_bysubtype(production,'Hydro')
# top10_bysubtype(production,'Wind')
# top10_bysubtype(production,'Geothermal')


# Who are the top 10 renewable energy producers Per Capita?
top10_re_pc <- aggregate(percapita ~ country, production_re, sum)
top10_re_pc <- top10_re_pc[order( -top10_re_pc[,'percapita'] ) , ]
top10_re_pc <- top10_re_pc[1:10,]
rownames(top10_re_pc) <- NULL


# Who are the top 10 renewable energy producers (by renewable subtype) Per Capita?
top10_bysubtype_pc <- function(df, st) {
  df.st <- df[df$subtype==st, ]
  df.st <- aggregate(percapita ~ country, df.st, sum)
  df.st <- df.st[order( -df.st[,'percapita'] ) , ]
  df.st <- df.st[1:10,]
  rownames(df.st) <- NULL
  return(df.st)
}

# top10_bysubtype_pc(production,'Solar')
# top10_bysubtype_pc(production,'Hydro')
# top10_bysubtype_pc(production,'Wind')
# top10_bysubtype_pc(production,'Geothermal')


###########################################################################################
# Top 5 Largest producters of total renewable electricity 

df.brazil <- production_re[production_re$country == 'Brazil',]
df.us <- production_re[production_re$country == 'United States',]
df.russia <- production_re[production_re$country == 'Russia',]
df.china <- production_re[production_re$country == 'China',]
df.canada <- production_re[production_re$country == 'Canada',]


###########################################################################################
# Largest producters of any given renewable electricity subtype
# Solar
# country quantity
# 1        Germany 517341.6
df.germany <- production_re[production_re$country == 'Germany',]

# Hydro
# country quantity
# 1          China 35684352
df.china <- production_re[production_re$country == 'China',]

# Wind
# country  quantity
# 1   United States 3608521.2
df.us <- production_re[production_re$country == 'United States',]

# Geothermal
# country  quantity
# 1  United States 1477278.0
# 2    Philippines  800632.8
df.philippines <- production_re[production_re$country == 'Philippines',]


#########################################################################################
# Top 5 Largest producters of total renewable electricity  Per Capita

# country percapita
# 70      Iceland 2.9442989
# 120      Norway 2.4163748
# 29       Canada 1.0142024
# 123    Paraguay 0.7809985
# 151      Sweden 0.6957508

df.iceland <- production_re[production_re$country == 'Iceland',]
df.norway <- production_re[production_re$country == 'Norway',]
df.canada <- production_re[production_re$country == 'Canada',]
df.paraguay <- production_re[production_re$country == 'Paraguay',]
df.sweden <- production_re[production_re$country == 'Sweden',]


###########################################################################################
# Largest producters of any given renewable electricity subtype Per Capita

# > top10_bysubtype_pc(production,'Solar')
# country   percapita
# 1        Germany 0.006379815
df.germany <- production_re[production_re$country == 'Germany',]

# > top10_bysubtype_pc(production,'Hydro')
# country percapita
# 1       Norway 2.4072675
df.norway <- production_re[production_re$country == 'Norway',]

# > top10_bysubtype_pc(production,'Wind')
# country  percapita
# 1        Denmark 0.08275895
df.denmark <- production_re[production_re$country == 'Denmark',]

# > top10_bysubtype_pc(production,'Geothermal')
# country   percapita
# 1        Iceland 0.612454348
df.iceland <- production_re[production_re$country == 'Iceland',]



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 2.1 - production renewable scatter
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


g.dot.renew <- production_re[ , which( names(production_re) %in%
                                         c('country'
                                           ,'year'
                                           ,'subtype'
                                           ,'continent'
                                           ,'hdi'
                                           ,'population'
                                           ,'percapita'
                                           ,'percapita_sum_y_re'
                                           ,'percapita_sum_y'
                                         ) ) ]

# g.pop <- g.dot.renew[,c('country','population')]
# g.pop <- unique(g.pop)
# g.pop <- g.pop[order(g.pop$population, decreasing = T),]
# g.pop <- g.pop[g.pop$population > 50000000,]

g.dot.renew <- g.dot.renew[g.dot.renew$year==2014,]
g.dot.renew <- g.dot.renew[, which( names(g.dot.renew) != 'year')]
g.dot.renew <- g.dot.renew[g.dot.renew$country != 'Saudi Arabia', ]
g.dot.renew <- g.dot.renew[g.dot.renew$population >= 10000000, ]
g.dot.renew <- unique(g.dot.renew)
g.dot.renew <- na.omit(g.dot.renew)
g.dot.renew$percapita_pct_y_re <- g.dot.renew$percapita_sum_y_re / g.dot.renew$percapita_sum_y



g.dot.renew.a <- g.dot.renew[, which( names(g.dot.renew) %in% 
                                        c('country'
                                          ,'continent'
                                          ,'population'
                                          ,'hdi'
                                          ,'percapita_sum_y_re'
                                          ,'percapita_sum_y'
                                          ,'percapita_pct_y_re')
) ]
g.dot.renew.a <- unique(g.dot.renew.a)
g.dot.renew.a[order(g.dot.renew.a$hdi),]


# write.csv(g.dot.renew.a, 'renew_analysis.csv')


#############################################


ggplot(g.dot.renew.a, aes(x=hdi, y=percapita_sum_y_re)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9), labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita Renewable use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')

ggplot(g.dot.renew.a, aes(x=hdi, y=percapita_sum_y)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita energy use in 2014 including non-renewable"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')

ggplot(g.dot.renew.a, aes(x=hdi, y=percapita_pct_y_re)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9), labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Renewable percentage of total energy use in 2014 "
       ,x='HDI'
       ,y='Energy per-capita (TJ)')



# These include subtype so we need that dimension to be included in the plot

ggplot(g.dot.renew, aes(x=hdi, y=percapita)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~subtype) +
  labs(title = "Per-capita Renewable use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')


# Create non-faceted subtype specific plots

ggplot(filter(g.dot.renew, subtype == "Wind"), aes(x=hdi, y=percapita)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita Wind use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')

ggplot(filter(g.dot.renew, subtype == "Solar"), aes(x=hdi, y=percapita)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita Solar use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')

ggplot(filter(g.dot.renew, subtype == "Geothermal"), aes(x=hdi, y=percapita)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita Geothermal use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')

ggplot(filter(g.dot.renew, subtype == "Hydro"), aes(x=hdi, y=percapita)) +
  geom_jitter(aes(size=population, col=continent)) + 
  scale_size_continuous(breaks=c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)
                        , labels = c(1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9)) +
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Per-capita Hydro use in 2014"
       ,x='HDI'
       ,y='Energy per-capita (TJ) Log 10')


# geom_smooth(aes(col=manufacturer), method="lm", se=F)

rm(g.dot.renew, g.dot.renew.a)  


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 2.2 - production renewable world lolipop
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

#source('data-3 top-10 countries.r')


top10_bysubtype <- function(df, st) {
  df.st <- df[df$subtype==st, ]
  df.st <- aggregate(quantity ~ country, df.st, sum)
  df.st <- df.st[order( -df.st[,'quantity'] ) , ]
  df.st <- df.st[1:10,]
  rownames(df.st) <- NULL
  return(df.st)
}

g.solar <- top10_bysubtype(production,'Solar')
g.hydro <- top10_bysubtype(production,'Hydro')
g.wind <- top10_bysubtype(production,'Wind')
g.geo <- top10_bysubtype(production,'Geothermal')

# g.geo$country <- factor(g.geo$country, levels = g.geo$country)


theme_set(theme_bw())
# display.brewer.all()
# filler <- brewer.pal(9, "RdYlBu")


g.render <- function(df, df.title) {
  ggplot(df, aes(x=reorder(country,quantity), y=quantity, label=quantity)) + 
    geom_point(stat='identity', aes(fill=quantity), size=6, shape=21)  +
    scale_fill_gradient(low = "red", high = "green", guide = guide_colorbar(title='TeraJoules') ) +
    geom_segment(aes(y = 0, 
                     x = country, 
                     yend = quantity, 
                     xend = country,
                     col=quantity), show.legend = FALSE ) +
    scale_colour_gradient(low = "red", high = "green") +
    labs(title=df.title, x=NULL, y=NULL) +
    coord_flip()
}

g.render(g.solar, 'Solar')
g.render(g.hydro, 'Hydro')
g.render(g.wind, 'Wind')
g.render(g.geo, 'Geothermal')

rm(g.solar, g.hydro, g.wind, g.geo)  


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 2.3 - production renewable world maps
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


g.renew <- production_re[ , which( names(production_re) %in%
                                     c('country'
                                       ,'subtype'
                                       ,'quantity'
                                       ,'map_country'
                                       ,'continent') ) ]


g.renew <- aggregate(quantity ~ ., g.renew, sum)
# g.renew <- aggregate(quantity ~ country + map_country + subtype, g.renew, sum)


g.renew.all <- g.renew[ , which( names(g.renew) %in%
                                   c('country'
                                     ,'quantity'
                                     ,'map_country'
                                     ,'continent') ) ]

g.renew.all <- aggregate(quantity ~ ., g.renew.all, sum)


theme_set(theme_light())  

global1<-map_data("world")
global2<-map_data("world2")
global3<-map_data("world", wrap=c(0,360))
global4<-map_data("world2", wrap=c(-180,180))





# World Maps by subtype

multi_map <- function(df, geomap, maptitle) {
  map <- ggplot()
  map <- map + geom_map(data=geomap, map=geomap, aes(group=group, map_id = region),fill='grey90',color='#7f7f7f')
  map <- map + geom_map(data = df, map=geomap, aes(fill=quantity, map_id=map_country))
  map <- map + scale_fill_gradientn(name = 'quantity'
                                    , colors = brewer.pal(9, "RdYlBu")
                                    , guide = guide_colorbar(title='TeraJoules'
                                                             # ,direction = 'horizontal'
                                    )
  )
  map <- map + geom_map(data=geomap, map=geomap, aes(group=group, map_id = region),fill=NA,color='#7f7f7f')
  map <- map + expand_limits(x = geomap$long, y = geomap$lat)
  map <- map + coord_map("mercator")
  map <- map + ylim(-60,100)
  map <- map + labs(title = paste("World", maptitle  ,"Electricity Production 1990-2014", sep = ' '))
  map <- map + labs(x = NULL)
  map <- map + labs(y = NULL)
  map <- map + theme(
    axis.title.x=element_blank()
    ,axis.text.x=element_blank()
    ,axis.ticks.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,panel.background = element_rect(fill = "aliceblue")
    ,plot.background = element_rect(fill = "grey88")
    ,legend.position=c( .075, .16)
    , legend.background=element_blank()
    , legend.key.height = unit(.4, "cm")
    , legend.key.width = unit(.7, "cm")
    #, legend.key.size = unit(.1, "cm")
    , legend.text = element_text(size =7)
    , legend.title = element_text(size =8)
  )
  map
}

g.renew.geo <- g.renew[g.renew$subtype == 'Geothermal', ]
g.renew.hydro <- g.renew[g.renew$subtype == 'Hydro', ]
g.renew.wind <- g.renew[g.renew$subtype == 'Wind', ]
g.renew.solar <- g.renew[g.renew$subtype == 'Solar', ]

multi_map(g.renew.geo, global4, maptitle = 'Geothermal')
multi_map(g.renew.hydro, global4, maptitle = 'Hydro')
multi_map(g.renew.wind, global4, maptitle = 'Wind')
multi_map(g.renew.solar, global4, maptitle = 'Solar')
multi_map(g.renew.all, global4, maptitle = 'Total Renewable')



# a <- multi_map(g.renew.geo, global4, maptitle = 'Geothermal')
# b <- multi_map(g.renew.hydro, global4, maptitle = 'Hydro')
# c <- multi_map(g.renew.wind, global4, maptitle = 'Wind')
# d <- multi_map(g.renew.solar, global4, maptitle = 'Solar')
# ggarrange(a,b,c,d, ncol = 2, nrow = 2)



# Facet maps by all renewables, not normalized for renewable type
map <- ggplot()
map <- map + geom_map(data=global4, map=global4, aes(group=group, map_id = region),fill='grey90',color='#7f7f7f')
map <- map + geom_map(data = g.renew, map=global4, aes(fill=quantity, map_id=map_country))
map <- map + scale_fill_gradientn(name = 'quantity'
                                  , colors = brewer.pal(9, "RdYlBu")
                                  , guide = guide_colorbar(title='TeraJoules'
                                                           # ,direction = 'horizontal'
                                  )
)
map <- map + geom_map(data=global4, map=global4, aes(group=group, map_id = region),fill=NA,color='#7f7f7f')
map <- map + expand_limits(x = global4$long, y = global4$lat)
map <- map + coord_map("mercator")
map <- map + ylim(-60,100)
map <- map + facet_wrap(~subtype) #, nrow = 4, ncol = 1)
map <- map + labs(title = "World Renewable Energy Production 1990-2014")
map <- map + labs(x = NULL)
map <- map + labs(y = NULL)
map <- map + theme(
  axis.title.x=element_blank()
  ,axis.text.x=element_blank()
  ,axis.ticks.x=element_blank()
  ,axis.text.y=element_blank()
  ,axis.ticks.y=element_blank()
)
map


theme_set(theme_gray())  
rm(global1,global2,global3,global4)
rm(g.renew, g.renew.geo, g.renew.hydro ,g.renew.solar ,g.renew.wind)


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 3.1 - consumption top10 bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

#source('data-3 top-10 countries.r')


fill <- brewer.pal(12, "Paired")

#color palette for 5 variables for renewable
nature5 <- c("#257DD6", "#FADC05", "#FA6105", "#ACFCF4", "#2625d6") 

#create color scheme for 8 variables for total energy
nature8 <- c("#D66825", "#D6C125", "#93D625", "#3BD625", "#25D668", "#25D6C1", "#257DD6", "#2625D6")

#create color scheme for 11 variables for total energy
nature11 <- c("#D6252E", "#FA5525", "#FABF25", "#FFC300", "#CAFA25", "#60FA25", "#25FA55", "#25FAC0", "#25B8FA", "#254DFA", "#6725FA")





# top 10 countries 100% fill sideways barchart
ggplot(top10_consumption) + 
  geom_bar(aes(x = reorder(country, sum_25y), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .7
           #, position = position_stack(reverse = F) ) +
  ) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()

# check the chart for correctness: print out some values to compare to 
# e.g. thermal for US looks like about 2.5E8 / 1.5E9 = 
# x <- 2.5E8 / 1.5E9  # about 16.5 %


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 3.2 - consumption top 10 barstack
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')

fill <- brewer.pal(12, "Paired")

#color palette for 5 variables for renewable
nature5 <- c("#257DD6", "#FADC05", "#FA6105", "#ACFCF4", "#2625d6") 

#create color scheme for 8 variables for total energy
nature8 <- c("#D66825", "#D6C125", "#93D625", "#3BD625", "#25D668", "#25D6C1", "#257DD6", "#2625D6")

#create color scheme for 11 variables for total energy
nature11 <- c("#D6252E", "#FA5525", "#FABF25", "#FFC300", "#CAFA25", "#60FA25", "#25FA55", "#25FAC0", "#25B8FA", "#254DFA", "#6725FA")



# top 10 countries, sideways barchart
ggplot(top10_consumption) + 
  geom_bar(aes(x = reorder(country, sum_25y), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity", width = .4
           , position = position_stack(reverse = F)) +
  labs(y= element_blank()) +
  scale_fill_manual(values=nature11) +
  coord_flip()


fill <- brewer.pal(12, "Paired")

# top 10 countries, sideways barchart
ggplot(top10_consumption) + 
  geom_bar(aes(x = reorder(country, sum_25y), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity", width = .4
           , position = position_stack(reverse = F)) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()


# check the chart for correctness: print out some values to compare to 
# e.g. thermal for US looks like about 2.5E8 and total us is about 1.5E9


us.sum_subtype <- top10_consumption[ , which( names(top10_consumption) %in%
                                                c('country','subtype','quantity')
) ]

us.sum_subtype <- us.sum_subtype %>% 
  filter(country=="United States") %>% 
  group_by(subtype) %>% 
  summarise(quantity=sum(quantity))

sum(us.sum_subtype$quantity)
rm(us.sum_subtype)


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 3.3 - consumption US x 5yr bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


# United States, past comparison by subtype each 5 years
us.4y <- top10_consumption[top10_consumption$country=='United States' 
                           & top10_consumption$year %in% c(1990,1995,2000,2005,2010,2014),]

us.4y$year <- as.factor(us.4y$year)
us.4y <- na.omit(us.4y)


fill <- brewer.pal(12, "Paired")


ggplot(us.4y) + 
  geom_bar(aes(x = year, y=quantity, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .4 
           # , position = position_stack(reverse = F) ) +
  ) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.1 - production top10 barstack bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


fill <- brewer.pal(12, "Paired")

# top 10 countries, sideways barchart
ggplot(top10_production) + 
  geom_bar(aes(x = reorder(country, sum_25y), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity", width = .4
           , position = position_stack(reverse = F)) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()

# top 10 countries 100% fill sideways barchart
ggplot(top10_production) + 
  geom_bar(aes(x = reorder(country, sum_25y), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .7
           #, position = position_stack(reverse = F) ) +
  ) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()



###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.2 - production renewable top5 bar (5)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


fill <- brewer.pal(12, "Paired")


col_plot <- function(df, chart.title = 'none') {
  a <- ggplot(df)
  a <- a + geom_col(aes(x = reorder(subtype, subtype_sum_25y), y=quantity, fill=subtype) 
                    ,width = .4)
  a <- a + labs(y= element_blank())
  a <- a + scale_fill_manual(values=fill)
  
  if (chart.title!='none') {a <- a + labs(
    title=paste(chart.title, "Production by Renewable Type 1990-2014", sep = " "),
    y="Terajoules (log10)",
    x="Renewable Type")
  }
  else {a <- a + labs(title="Production by Renewable Type 1990-2014")}
  
  # a <- a + theme(panel.background = element_rect(fill = "black") )
  # coord_flip()
}



###########################################################################################
# Top 5 Largest producters of total renewable electricity 

g.brazil <- col_plot(df.brazil, "Brazil")
g.us <- col_plot(df.us, "United States")
g.russia <- col_plot(df.russia, "Russia")
g.china <- col_plot(df.china, "China")
g.canada <- col_plot(df.canada, "Canada")

g.brazil
g.us
g.russia
g.china
g.canada

rm(g.brazil, g.us, g.russia, g.china, g.canada)


###########################################################################################
# Largest producters of any given renewable electricity subtype
# Solar
# country quantity
# 1        Germany 517341.6
g.germany <- col_plot(df.germany, "Germany")

# Hydro
# country quantity
# 1          China 35684352
g.china <- col_plot(df.china, "China")

# Wind
# country  quantity
# 1   United States 3608521.2
g.us <- col_plot(df.us, "United States")

# Geothermal
# country  quantity
# 1  United States 1477278.0
g.us <- col_plot(df.us, "United States")

g.germany
g.china
g.us

rm(g.germany, g.china, g.us)

#########################################################################################
# Top 5 Largest producters of total renewable electricity  Per Capita

# country percapita
# 70      Iceland 2.9442989
# 120      Norway 2.4163748
# 29       Canada 1.0142024
# 123    Paraguay 0.7809985
# 151      Sweden 0.6957508

g.iceland <- col_plot(df.iceland, "Iceland")
g.norway <- col_plot(df.norway, "Norway")
g.canada <- col_plot(df.canada, "Canada")
g.paraguay <- col_plot(df.paraguay, "Paraguay")
g.sweden <- col_plot(df.sweden, "Sweden")

g.iceland
g.norway
g.canada
g.paraguay
g.sweden

rm(g.iceland, g.norway, g.canada, g.paraguay, g.sweden)


###########################################################################################
# Largest producters of any given renewable electricity subtype Per Capita

# > top10_bysubtype_pc(production,'Solar')
# country   percapita
# 1        Germany 0.006379815
g.germany <- col_plot(df.germany, "Germany")

# > top10_bysubtype_pc(production,'Hydro')
# country percapita
# 1       Norway 2.4072675
g.norway <- col_plot(df.norway, "Norway")

# > top10_bysubtype_pc(production,'Wind')
# country  percapita
# 1        Denmark 0.08275895
g.denmark <- col_plot(df.denmark, "Denmark")

# > top10_bysubtype_pc(production,'Geothermal')
# country   percapita
# 1        Iceland 0.612454348
g.iceland <- col_plot(df.iceland, "Iceland")

g.germany
g.norway
g.denmark
g.iceland

rm(g.germany, g.norway, g.denmark, g.iceland)


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.3 - production renewable top10 bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


g.top10_re <- top10_re[ , which( names(top10_re) %in%
                                   c('country'
                                     ,'subtype'
                                     ,'map_country'
                                     ,'continent'
                                     ,'sum_25y_re'
                                     ,'subtype_sum_25y') ) ]
g.top10_re <- unique(g.top10_re)


fill <- brewer.pal(12, "Paired")
fill <- c("orangered3","dodgerblue3","mediumpurple2","grey51","burlywood1","olivedrab3","rosybrown1")

# top 10 countries 100% fill sideways barchart
ggplot(g.top10_re) + 
  geom_bar(aes(x = reorder(country, sum_25y_re), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .7
           #, position = position_stack(reverse = F) ) +
  ) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.4 - production renewable top10 barstack - 3 ways to do it
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


g.top10_re <- top10_re[ , which( names(top10_re) %in%
                                   c('country'
                                     ,'subtype'
                                     ,'map_country'
                                     ,'continent'
                                     ,'sum_25y_re'
                                     ,'subtype_sum_25y') ) ]

g.top10_re <- unique(g.top10_re)


fill <- brewer.pal(12, "Paired")
# fill <- c("orangered3","dodgerblue3","mediumpurple2","grey51","burlywood1","olivedrab3","rosybrown1")


# compare different ways of doing this.

# Method 1 ##########################################
# top 10 countries, sideways barchart
ggplot(g.top10_re) + 
  geom_bar(aes(x = reorder(country, sum_25y_re), y=subtype_sum_25y, fill=subtype) 
           , stat = "identity", width = .4
           , position = position_stack(reverse = F)) +
  labs(y= element_blank()) +
  scale_fill_manual(values=fill) +
  coord_flip()

# 
# # Method 2 ##########################################
# # top 10 countries, sideways barchart
# ggplot(top10_re) + 
#   geom_bar(aes(x = reorder(country, sum_25y_re), y=quantity, fill=subtype) 
#            , stat = "identity", width = .4
#            , position = position_stack(reverse = F)) +
#   labs(y= element_blank()) +
#   scale_fill_manual(values=fill) +
#   coord_flip()
# 
# 
# # Method 3 ##########################################
# q.agg <- aggregate(quantity~country,top10_re,sum)
# q.agg <- q.agg[order(q.agg$quantity,decreasing = F),]
# 
# top10_re$country <- factor(top10_re$country, levels = q.agg$country)
# levels(top10_re$country)
# 
# # top 10 countries, sideways barchart
# ggplot(top10_re) + 
#   geom_bar(aes(x = country, y=quantity, fill=subtype) 
#            , stat = "identity", width = .4
#            , position = position_stack(reverse = F)) +
#   labs(y= element_blank()) +
#   scale_fill_manual(values=fill) +
#   coord_flip()

# Clean up vars
rm(g.top10_re)


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.5 - production renewable top10 x5yr bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################


# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


top10_5yr <- top10_re[top10_re$year %in% c(1990,1995,2000,2005,2010,2014),]


top10_5yr$year <- as.factor(top10_5yr$year)
top10_5yr <- na.omit(top10_5yr)


fill <- brewer.pal(12, "Paired")


ggplot(top10_5yr) +
  geom_bar(aes(x = year, y=quantity, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .9 ) +
  facet_wrap(~country) + #, nrow = 4, ncol = 1)
  labs(x='year', title = 'Top 10 Producers, 1990-2014', subtitle = 'Renewable % by Type', fill = "Renewable") +
  scale_fill_manual(values=fill) +
  theme(
    axis.title.x=element_blank()
    ,axis.text.x=element_blank()
    ,axis.ticks.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
  )



top10_5yr.ex.hyd <- top10_5yr[top10_5yr$subtype != 'Hydro', ]

ggplot(top10_5yr.ex.hyd) +
  geom_bar(aes(x = year, y=quantity, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = .9 ) +
  facet_wrap(~country) + #, nrow = 4, ncol = 1)
  labs(x='year', title = 'Top 10 Producers, 1990-2014', subtitle = 'Renewable % by Type', fill = "Renewable") +
  scale_fill_manual(values=fill) +
  theme(
    axis.title.x=element_blank()
    ,axis.text.x=element_blank()
    ,axis.ticks.x=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
  )


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 4.6 - production renewable US x5yr bar100
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')


us.5y.renew <- top10_re[top10_re$country=='United States' 
                        & top10_re$year %in% c(1990,1995,2000,2005,2010,2014),]

fill <- brewer.pal(4, "Paired")

ggplot(us.5y.renew) + 
  geom_bar(aes(x = year, y=quantity, fill=subtype) 
           , stat = "identity"
           , position = "fill"
           , width = 3 ) +
  labs(x='year'
       , title = 'United States, 1990-2014'
       , subtitle = 'Renewable % by Type'
       , fill = "Renewable") +
  scale_fill_manual(values=fill) +
  coord_flip()


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 5.1 - production renewable SUBTYPE lineplots(3)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################



# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

# source('data-3 top-10 countries.r')



###############################################################################################

quantity_wide <- function(df) {
  g.quantity <- df[,which( names(df) %in% c('year','subtype','quantity') ) ]
  
  g.quantity <- aggregate(quantity ~ ., g.quantity, sum)
  g.quantity.wide <- spread(g.quantity,subtype,quantity)
  g.quantity.wide$total <- apply(g.quantity.wide[,2:length(g.quantity.wide)], 1, function(x) sum(x, na.rm=T) )
  
  return(g.quantity.wide)
}     

quantity_wide_p <- function(g.quantity.wide) {     
  
  g.quantity.wide.p <- g.quantity.wide
  if ('Geothermal' %in% names(g.quantity.wide.p)) {g.quantity.wide.p$Geothermal <- 100*(g.quantity.wide.p$Geothermal / g.quantity.wide.p$total)}
  if ('Hydro' %in% names(g.quantity.wide.p)) {g.quantity.wide.p$Hydro <- 100*(g.quantity.wide.p$Hydro / g.quantity.wide.p$total)}
  if ('Solar' %in% names(g.quantity.wide.p)) {g.quantity.wide.p$Solar <- 100*(g.quantity.wide.p$Solar / g.quantity.wide.p$total)}
  if ('Wind' %in% names(g.quantity.wide.p)) {g.quantity.wide.p$Wind <- 100*(g.quantity.wide.p$Wind / g.quantity.wide.p$total)}
  
  g.quantity.wide <- g.quantity.wide[, -which( names(g.quantity.wide)=='total') ]
  g.quantity.wide.p <- g.quantity.wide.p[, -which( names(g.quantity.wide.p)=='total') ]
  
  return(g.quantity.wide.p)
}



###############################################################################################
percapita_wide <- function(df) {
  
  g.percapita <- df[,which( names(df) %in% c('year','subtype','percapita') ) ]
  
  g.percapita <- aggregate(percapita ~ ., g.percapita, sum)
  g.percapita.wide <- spread(g.percapita,subtype,percapita)
  g.percapita.wide$total <- apply(g.percapita.wide[,2:5], 1, function(x) sum(x, na.rm=T) )
  
  return(g.percapita.wide)
}     

percapita_wide_p <- function(g.percapita.wide) {   
  
  g.percapita.wide.p <- g.percapita.wide
  
  g.percapita.wide.p$Geothermal <- 100*(g.percapita.wide.p$Geothermal / g.percapita.wide.p$total)
  g.percapita.wide.p$Hydro <- 100*(g.percapita.wide.p$Hydro / g.percapita.wide.p$total)
  g.percapita.wide.p$Solar <- 100*(g.percapita.wide.p$Solar / g.percapita.wide.p$total)
  g.percapita.wide.p$Wind <- 100*(g.percapita.wide.p$Wind / g.percapita.wide.p$total)
  
  g.percapita.wide <- g.percapita.wide[, -which( names(g.percapita.wide)=='total') ]
  g.percapita.wide.p <- g.percapita.wide.p[, -which( names(g.percapita.wide.p)=='total') ]
  
  return(g.percapita.wide.p)
}

###############################################################################################


g.quantity.wide <- quantity_wide(production_re)
g.quantity.wide.p <- quantity_wide_p(g.quantity.wide)

g.percapita.wide <- percapita_wide(production_re)
g.percapita.wide.p <- percapita_wide_p(g.percapita.wide)



# theme_set(theme_grey())
# theme_set(theme_gray())
# theme_set(theme_bw())
# theme_set(theme_linedraw())
theme_set(theme_light())
# theme_set(theme_dark())
# theme_set(theme_minimal())  
# theme_set(theme_classic())  
# theme_set(theme_void())
# theme_set(theme_test()) 

# nature4 <- c("#257DD6", "#FADC05", "#FA6105", "#ACFCF4") 




###############################################################################################

# Quantity, linear scale
l.linear <- function(g.quantity.wide, chart.title='none') {
  
  a <- ggplot(g.quantity.wide, aes(x=year))
  if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production by Renewable Type 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production by Renewable Type 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green")) 
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  
  return(a)
}

# Quantity, log10 scale
l.log10 <- function(g.quantity.wide, chart.title='none') {
  a <- ggplot(g.quantity.wide, aes(x=year))
  if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production by Renewable Type (log10) 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production by Renewable Type (log10) 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_y_continuous(trans = "log10")
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green")) 
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  return(a)
}


# Percent Quantity, linear scale
l.linear_pct <- function(g.quantity.wide.p, chart.title='none') {
  a <- ggplot(g.quantity.wide.p, aes(x=year))
  if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production Percentage by Renewable Type 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production Percentage by Renewable Type 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green"))
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  return(a)
}


a <- l.linear(g.quantity.wide)
b <- l.log10(g.quantity.wide)
c <- l.linear_pct(g.quantity.wide.p)

a
b
c


###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  graphics - 5.2 - production renewable country+subtype lineplots
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################
###################################################################################
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#  This section contains some errors and is still under construction 
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
###################################################################################

# Note - working directory should be same as source code for all scripts
#setwd('yourdirectory')
getwd()

#source('gr d5 prod_re subtype lineplots(3).r')

theme_set(theme_light())


###########################################################################################
# Top 5 Largest producters of total renewable electricity 

# df.brazil
# df.us
# df.russia
# df.china
# df.canada


g.quantity.wide <- quantity_wide(df.us)
country.title <- 'United States'
l.linear(g.quantity.wide, chart.title = country.title)
l.log10(g.quantity.wide, chart.title = country.title)
l.linear_pct(g.quantity.wide.p, chart.title = country.title)

g.quantity.wide <- quantity_wide(df.russia)
country.title <- 'Russia'
l.linear(g.quantity.wide, chart.title = country.title)
l.log10(g.quantity.wide, chart.title = country.title)
l.linear_pct(g.quantity.wide.p, chart.title = country.title)

g.quantity.wide <- quantity_wide(df.china)
country.title <- 'China'
l.linear(g.quantity.wide, chart.title = country.title)
l.log10(g.quantity.wide, chart.title = country.title)
l.linear_pct(g.quantity.wide.p, chart.title = country.title)

g.quantity.wide <- quantity_wide(df.canada)
country.title <- 'Canada'
l.linear(g.quantity.wide, chart.title = country.title)
l.log10(g.quantity.wide, chart.title = country.title)
l.linear_pct(g.quantity.wide.p, chart.title = country.title)



###############################################################################################
# Brazil only - redefine function (for sake of time) to avoid error from not having Geothermal data
# By commenting out one line.

# Quantity, linear scale
l.linear <- function(g.quantity.wide, chart.title='none') {
  
  a <- ggplot(g.quantity.wide, aes(x=year))
  # if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production by Renewable Type 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production by Renewable Type 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green")) 
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  
  return(a)
}

# Quantity, log10 scale
l.log10 <- function(g.quantity.wide, chart.title='none') {
  a <- ggplot(g.quantity.wide, aes(x=year))
  # if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production by Renewable Type (log10) 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production by Renewable Type (log10) 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_y_continuous(trans = "log10")
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green")) 
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  return(a)
}


# Percent Quantity, linear scale
l.linear_pct <- function(g.quantity.wide.p, chart.title='none') {
  a <- ggplot(g.quantity.wide.p, aes(x=year))
  # if ('Geothermal' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Geothermal, col="geothermal"), size = 1, linetype="dashed")}
  if ('Hydro' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Hydro, col="hydro"), size = 1, linetype="dotdash")}
  if ('Solar' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Solar, col="solar"), size = 1, linetype="solid")}
  if ('Wind' %in% names(g.quantity.wide.p)) {a <- a + geom_line(aes(y=Wind, col="wind"), size = 1, linetype="twodash")}
  
  if (chart.title!='none') {a <- a + labs(title=paste(chart.title, "Production Percentage by Renewable Type 1990-2014", sep = " "), 
                                          # subtitle="1990-2014", 
                                          y="Terajoules (log10)")}
  else {a <- a + labs(title="Production Percentage by Renewable Type 1990-2014", 
                      # subtitle="1990-2014", 
                      y="Terajoules (log10)")}
  
  a <- a + scale_color_manual(name="Renewable"
                              ,labels = names(g.quantity.wide[,2:length(g.quantity.wide)])
                              ,values = c("geothermal"="red"
                                          , "hydro"="blue"
                                          , "solar"="orange"
                                          , "wind"="green"))
  a <- a + theme(panel.background = element_rect(fill = "aliceblue") )
  return(a)
}


g.quantity.wide <- quantity_wide(df.brazil)
country.title <- 'Brazil'
l.linear(g.quantity.wide, chart.title = country.title)
l.log10(g.quantity.wide, chart.title = country.title)
l.linear_pct(g.quantity.wide.p, chart.title = country.title)

rm(l.linear, l.log10, l.linear_pct)






