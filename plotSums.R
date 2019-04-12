## Script for plotting GHG-emissions
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
outdate = "2019-04-10"

## Setup filenames
fname_in_AE <- paste0(inddir,"KPI_aGHG", "_", indate,".csv")
fname_out_custPF <- paste0(clndir,"CustPortfolio", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- AE/AEyr ------ Creating and Plotting GHG Emission data ------------
## ----------------------------------------------------------------------- 
## 1 - read file from csv
AE <- read.csv2(fname_in_AE,
                stringsAsFactors = FALSE,
                fileEncoding = "UTF-8")
AE <- AE[!is.na(AE$Ems),]

## 2 - multiply emissions by factor to shift sum into allowance
## we still need to recalculate the original xls that has been altered
newtot <- 3.37862339163655
oldtot <- sum (AE$Ems, na.rm = TRUE) / 10^6
shift <- newtot / oldtot

AEs <- AE %>%
  mutate(Scope_1 <- Scope_1 * shift) %>%
  mutate(Scope_2 <- Scope_2 * shift) %>%
  mutate(Ems <- Ems * shift) %>%
  mutate(NetPort <- NetPort * shift)


  