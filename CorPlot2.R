## Script for overview of correlations
## GVersteeg, April 10th, 2019
##
## IN/OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : KPI_aGHG.csv                    (247 / 7 )                -- 
## ----------------------------------------------------------------------- 
##
## ----------------------------------------------------------------------- 
## --- ORGANIZE PROCESSING ENVIRONMENT ----------------------------------- 
## ----------------------------------------------------------------------- 
## Loading libraries
## Note: require is used inside functions, as it outputs a warning and 
## continues if the package is not found, whereas library will throw an error
library(tidyverse)
library(GGally)

## Setup locations
insite <- ""
rawdir <- "datalake/raw_data/"
lutdir <- "datalake/luts/"
clndir <- "datalake/clean_data/"
valdir <- "datalake/valid_data/"
inddir <- "datalake/indicators/"
dptdir <- "datalake/data_points/"
laydir <- "datalake/infoproducts/GIS_layers/"
qlkdir <- "datalake/infoproducts/Qlik_cubes/"
sopdir <- "datalake/infoproducts/sopact_sheets/"

## Setup parameters
version <- ""
indate = "2019-03-31"
lutdate = ""
outdate = ""

## Setup filenames
fname_in_AE <- paste0(inddir,"KPI_aGHG", "_", indate,".csv")
fname_in_custG <- paste0(clndir,"CustFac_General", "_", indate,".csv")
fname_in_lutC <- paste0(lutdir,"Map_CntrReg", "_", indate,".csv")
fname_in_IMF <- paste0(lutdir,"IMF_countries", "_", indate,".csv")

## ----------------------------------------------------------------------- 
## --- AE/AEyr ------ Reading AE's and Plotting correlations  ------------
## ----------------------------------------------------------------------- 
## 1 - read file from indicators
AE <- read.csv2(fname_in_AE,
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
AE <- AE[!is.na(AE$Ems),]

## 2 - link Client-country to customer in AE
custG <- read.csv2(fname_in_custG,
                stringsAsFactors = FALSE,
                fileEncoding = "UTF-8")
Base <- mutate(AE, Country = NA)
ix <- Base$Customer_ID %in% custG$Customer_ID
for (i in seq_along(Base$Customer_ID)) {
  if(ix){
    Base$Country[i] <- custG$Country[
      custG$Customer_ID == Base$Customer_ID[i]]
    }  
  }

## 3 - translate client-country to GHG_Country in AE
lutC <- read.csv2(fname_in_lutC,
                   stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8")
Base <- mutate(Base, GHG_Country = NA)
ix <- Base$Country %in% lutC$Country
for (i in seq_along(Base$Customer_ID)) {
  if(ix){
    Base$GHG_Country[i] <- lutC$Model_Region[
    lutC$Country == Base$Country[i]]
  }
}
Base <- select(Base, -Country)

## 4 - add IMF GHG_Country data to AE
IMF <- read.csv2(fname_in_IMF,
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8")
Base <- Base %>%
  mutate(GDP = NA, Inflation = NA, Population = NA,
         Unemployment = NA, OilImport = NA)
ix <- Base$GHG_Country %in% IMF$GHG_Country
for (i in seq_along(Base$Customer_ID)) {
  if(ix){
    Base$GDP[i] <- IMF$GDP[IMF$GHG_Country == Base$GHG_Country[i]]
    Base$Inflation[i] <- IMF$Inflation[IMF$GHG_Country == Base$GHG_Country[i]]
    Base$Population[i] <- IMF$Population[IMF$GHG_Country == Base$GHG_Country[i]]
    Base$Unemployment[i] <- IMF$Unemployment[IMF$GHG_Country == Base$GHG_Country[i]]
    Base$OilImport[i] <- IMF$OilImport[IMF$GHG_Country == Base$GHG_Country[i]]
    }
  }

## 3 - clean dataset & plot correlations
AEsel <- Base %>%
  filter(Ems < 10000) %>%
  select(-Customer_ID) %>%
  select(Ems, NetPort, Scope_1, Scope_2, GDP, Inflation,
         Population, Unemployment, OilImport)

ggpairs(AEsel)
