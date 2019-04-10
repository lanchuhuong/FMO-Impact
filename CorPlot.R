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
outdate = "2019-04-10"

## Setup filenames
fname_in_AE <- paste0(inddir,"KPI_aGHG", "_", indate,".csv")

## ----------------------------------------------------------------------- 
## --- AE/AEyr ------ Reading AE's and Plotting correlations  ------------
## ----------------------------------------------------------------------- 
## 1 - read file from indicators
AE <- read.csv2(fname_in_AE,
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
AE <- AE[!is.na(AE$Ems),]

## 2 - clean dataset & plot correlations
AEsel <- AE %>%
  select(-Customer_ID) %>%
  select(NetPort, Ems, Scope_1, Scope_2, Dep, Method) %>%
  filter(Dep != "FMO Investment Management") %>%
  filter(Ems < 100000)
AEsel$Dep[AEsel$Dep == "Infrastructure, Manufacturing & Services"] <- "IM&S" 
AEsel$Dep[AEsel$Dep == "Financial Institutions"] <- "FI" 
AEsel$Dep[AEsel$Dep == "Special Operations"] <- "SpecOps" 
AEsel$Dep[AEsel$Dep == "Private Equity"] <- "PrEq" 
AEsel$Dep[AEsel$Dep == "Agribusiness"] <- "Agri" 

ggpairs(AEsel) +
  theme(axis.text.x = element_text(angle = -90, 
                                   hjust = 0,
                                   vjust = 0.5))
