## Script for reading Gender-datasets from Excel-sheet
## GVersteeg, April 7th, 2019
##
## DESCRIPTION
## This temporary script reads Excel-sheet: Gender Indicators.xlsx
## and retrieves multiple data-scources, lookup-tables (lut's) and data-points.
## The Excel-sheet is read only for actually entered values, all calculated
## data in the Excel is disregarded. 
##
## Note:
## This script needs to be adapted for reading actual data from systems like
## ACBS-data (Investments per Facility) and FIA-data (Contributions per Customer)
##
## IN/OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : Gender Indicators.xlsx          / Input_Data_FMO          -- 
## -- LUT's :                                                           -- 
## -- Output: CustPortfolio.csv                 (1170   / 65)           -- 
## --       : IC_General.csv                    (31298  / 17)           -- 
## --       : GenderIndicators.csv              (740    / 28)           -- 
## ----------------------------------------------------------------------- 
##
## ----------------------------------------------------------------------- 
## --- ORGANIZE PROCESSING ENVIRONMENT ----------------------------------- 
## ----------------------------------------------------------------------- 
## Loading libraries
## Note: require is used inside functions, as it outputs a warning and 
## continues if the package is not found, whereas library will throw an error
library(tidyverse)
library(readxl)

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
indate = "2019-04-07"
lutdate = ""
outdate = "2019-04-07"

## Setup filenames
fname_in_raw <- paste0(rawdir,"Gender_Indicators", "_", indate,".xlsx")
fname_out_custPF <- paste0(clndir,"CustPortfolio", "_", outdate,".csv")
fname_out_icGen <- paste0(clndir,"IC_General", "_", outdate,".csv")
fname_out_GenInd <- paste0(clndir,"GenderIndicators", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- custPF ----------- READ Portfolio data FROM XLS -------------------
## ----------------------------------------------------------------------- 
## 1 - read file from xlsx-tab, remove last line (totals)
raw <- read_xlsx(fname_in_raw, sheet = "Dashboard Portfolio Detailed",
                 skip = 3)
raw <- raw[-nrow(raw),]

hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("%", "Perc", hdrs)
hdrs <- gsub("adj.", "adj", hdrs)
colnames(raw) <- hdrs
rm(hdrs)

custPF <- raw
write.csv2(custPF, fname_out_custPF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- icGen ----------- READ ImpactCard data FROM XLS -------------------
## ----------------------------------------------------------------------- 
## 1 - read file from xlsx-tab
v_coltype <- c("text", "text", "text", "text", "text", "text",
               "text", "text", "text", "date", "text","text",
               "text", "text", "text", "text", "text")
raw <- read_xlsx(fname_in_raw, sheet = "Impact Card data",
                 skip = 1, col_types = v_coltype)

hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("-", "_", hdrs)
hdrs <- gsub("e/", "e_", hdrs)
colnames(raw) <- hdrs
rm(hdrs)

## correct the status Active/Not-Active to Impact Card
match <- raw$Customer_Number %in% custPF$Customer_Id
for (i in seq_along(raw$Customer_Number)) {
  curcust <- raw$Customer_Number[i]
  raw$Active_Non_Active[i] <- ifelse(!match[i],"NOT ACTIVE",
      custPF$Overall_Status_Description[custPF$Customer_Id == curcust])
  }
icGen <- raw
write.csv2(icGen, fname_out_icGen, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- GenInd --------- READ Gender Indicators & RI data FROM XLS --------
## ----------------------------------------------------------------------- 
## 1 - read file from xlsx-tab
raw <- read_xlsx(fname_in_raw, sheet = "Gender_Indicators_and_RI_Label",
                 skip = 1)

hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("-", "_", hdrs)
hdrs <- gsub("\\(", "", hdrs)
hdrs <- gsub("\\)", "", hdrs)
hdrs <- gsub("___", "_", hdrs)
hdrs <- gsub("_&_", "_", hdrs)
hdrs <- gsub("\\#", "nr", hdrs)
hdrs <- gsub("\\/", "_", hdrs)
hdrs <- gsub("\\:", "", hdrs)
hdrs <- gsub("\\%", "Perc", hdrs)
colnames(raw) <- hdrs
rm(hdrs)

## 2 - remove calculated columns
# raw <- raw %>%
#   select(-`GL-UNIT (MASSIF)`) %>%
#   select(-`Department`) %>%
#   select(-`All Direct Employment combined`) %>%
#   select(-`# Direct Employment (operations and maintainence) for women`) %>%
#   select(-`Total # Depsitos`) %>%
#   select(-`Number of female depositors (#)`) %>%
#   select(-`Volume of loans for women-owned SMEs (EUR)`) %>%
#   select(-`Volume of micro enterprise loans for women (EUR)`) %>%
#   select(-`Total Volume of Micro Loans`) %>%
#   select(-`Active/Inactive account`) %>%
#   select(-`Archived version description`) %>%
#   select(-`Archive date Impact tab`) %>%
#   select(-`DISUS: Inclusive category - gender component`) %>%
#   select(-`RI Label: LDC %`) %>%
#   select(-`Total gross loan portfolio`) %>%
#   select(-`Strategic Sector`) %>%
#   select(-`Impact Card Type`) %>%
#   select(-`Direct Employment (PE - Investees Data)`) %>%
#   select(-`Industry Class Desc`)
  
## 3 - remove rows with and after "Totals" in Customer Number
begin <- which(raw$Customer_Number %in% "Totals")
eind <- nrow(raw)
raw <- raw[-c(begin:eind),]

## 4 - write source data
GenInd <- raw
write.csv2(GenInd, fname_out_GenInd, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
