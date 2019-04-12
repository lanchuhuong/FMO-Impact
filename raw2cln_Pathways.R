## Script for reading Input-datasets from Excel-sheet
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This temporary script reads Excel-sheet: Absolute GHG Accounting_SP.xlsx
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
## -- Input : Absolute GHG Accounting_SP.xlsx / Input_Data_FMO          -- 
## -- LUT's :                                                           -- 
## -- Output: CustFac_General_20190331.csv      (247    / 18)           -- 
## --       : IC_Corporate_20190331.csv         (48     / 7 )           -- 
## --       : IC_ProjFin_20190331.csv           (59     / 6 )           -- 
## --       : IC_FinInst_20190331.csv           (101    / 23)           -- 
## --       : IC_PrivEq_20190331.csv            (39     / 43)           -- 
## --       : PEF_Investments_20190331.csv      (117    / 5 )           -- 
## --       : PEF_RegionPerc_20190331.csv       (18     / 3 )           -- 
## --       : Fctr_EcEmis1_20190331.csv         (23     / 19)           -- 
## --       : Fctr_EcEmis2_20190331.csv         (23     / 19)           -- 
## --       : Fctr_CapInt_20190331.csv          (23     / 19)           -- 
## --       : Fctr_CBbrkdwn_20190331.csv        (23     / 17)           -- 
## --       : Fctr_Out2Elec_20190331.csv        (23     / 17)           -- 
## --       : MAP_CntrReg_20190331.csv          (87     / 2 )           -- 
## --       : MAP_ICSctrMdl_20190331.csv        (18     / 2 )           -- 
## --       : MAP_SctrMdl_20190331.csv          (21     / 2 )           -- 
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
library(stringr)


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
indate = ""
lutdate = ""
outdate = "2019-03-31"

## Setup filenames
fname_in_raw <- paste0(rawdir,"Added Value.xlsx")
fname_out_PWA <- paste0(clndir,"Pathway_Allow", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- PWA ---- Pathways and Allowances ----------------------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Added_Value!C32:AL38")

## 2 - rename column hdrs for easy processing
colnames(raw)[1] <- "type"

## 3 - wegschrijven CSV-file in datalake/clean_data
PWA <- raw
write.csv2(PWA, fname_out_PWA, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

