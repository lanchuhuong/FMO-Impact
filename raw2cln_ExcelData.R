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
## --       : Fctr_EcEmis_20190331.csv          (23     / 17)           -- 
## --       : Fctr_CapInt_20190331.csv          (23     / 17)           -- 
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

## Source micro-services
## 1. get-deviation-classes is needed for ....
## source("microservices.R")

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
fname_in_raw <- paste0(rawdir,"Absolute GHG Accounting", indate,".xlsx")
fname_out_custG <- paste0(clndir,"CustFac_General", "_", outdate,".csv")
fname_out_ICCp <- paste0(clndir,"IC_Corporate", "_", outdate,".csv")
fname_out_ICPF <- paste0(clndir,"IC_ProjFin", "_", outdate,".csv")
fname_out_ICFI <- paste0(clndir,"IC_FinInst", "_", outdate,".csv")
fname_out_ICPE <- paste0(clndir,"IC_PrivEq", "_", outdate,".csv")
fname_out_PEFInv <- paste0(clndir,"PEF_Investments", "_", outdate,".csv")
fname_out_EcEF <- paste0(clndir,"Fctr_EcEmis", "_", outdate,".csv")
fname_out_CpIF <- paste0(clndir,"Fctr_CapInt", "_", outdate,".csv")
fname_out_CBBD <- paste0(clndir,"Fctr_CBbrkdwn", "_", outdate,".csv")
fname_out_OutElc <- paste0(clndir,"Fctr_Out2Elec", "_", outdate,".csv")
fname_out_Map_CntrReg <- paste0(lutdir,"Map_CntrReg", "_", outdate,".csv")
fname_out_Map_SctrMdl <- paste0(lutdir,"Map_SctrMdl", "_", outdate,".csv")
fname_out_Map_ICSctrMdl <- paste0(lutdir,"Map_ICSctrMdl", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- INV/FAC ---------- READ ACBS/FIA-DATA OUT OF XLS INTO CSV ---------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, sheet = "Input_Data_FMO",
                 skip = 6)
## 2 - rename column-names for easy processing
hdrs <- colnames(raw)
hdrs <- gsub("-", "_", hdrs)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("___", "_", hdrs)
hdrs[c(16:17)] <- paste0("GNic_",hdrs[c(16:17)])
hdrs[c(19:24)] <- paste0("CPic_",hdrs[c(19:24)])
hdrs[c(26:30)] <- paste0("PFic_",hdrs[c(26:30)])
hdrs[c(32:54)] <- paste0("FIic_",hdrs[c(32:54)])
colnames(raw) <- hdrs
rm(hdrs)

## 3 - Split up GNic-type into sector/fintype
pattern <- "\xE2\x80\x93"
raw <- separate(raw, GNic_Impact_Card_Type, 
                 into = c("GNic_Type_sector", "GNic_Type_fin"),
                 sep = pattern, fill = "left")
raw$GNic_Type_fin <- sub(" corporate", "corporate", raw$GNic_Type_fin)
raw$GNic_Type_fin <- sub(" project finance", "project finance", raw$GNic_Type_fin)

## 4 - classify variables
raw$Department <- as.factor(raw$Department)
raw$Country <- as.factor(raw$Country)
raw$Client_sector <- as.factor(raw$Client_sector)
raw$GNic_Type_sector <- as.factor(raw$GNic_Type_sector)
raw$GNic_Type_fin <- as.factor(raw$GNic_Type_fin)
raw$GNic_Impact_Card_status <- as.factor(raw$GNic_Impact_Card_status)

## 5 - skip all empty columns (named: X__9)
raw <- select(raw, -starts_with("X__"))

## 6 - split file into general customer info and impact cards
## generate general customer info (ACBS/FIA)
custG <- raw[,c(1:17,30)]
colnames(custG)[15] <- "IC_sector"
colnames(custG)[16] <- "IC_type"
colnames(custG)[17] <- "IC_status"
colnames(custG)[18] <- "IC_prcGreenInv"
write.csv2(custG, fname_out_custG, row.names = FALSE,
           fileEncoding = "UTF-8")

## list candidates for impact cards to check completeness
table(raw$GNic_Type_fin)

## generate corporate impact card
rawsel <- filter(raw, as.character(GNic_Type_fin) == "corporate")
ICCp <- rawsel %>%
  select(starts_with("CPic_"))
ICCp <- cbind(rawsel[,1],ICCp)
colnames(ICCp) <- sub("CPic_", "", colnames(ICCp))
write.csv2(ICCp, fname_out_ICCp, row.names = FALSE,
           fileEncoding = "UTF-8")

## generate project finance impact card
rawsel <- filter(raw, as.character(GNic_Type_fin) == "project finance")
ICPF <- rawsel %>%
  select(starts_with("PFic_"))
ICPF <- cbind(rawsel[,1],ICPF)
colnames(ICPF) <- sub("PFic_", "", colnames(ICPF))
write.csv2(ICPF, fname_out_ICPF, row.names = FALSE,
           fileEncoding = "UTF-8")

## generate financial institution impact card
rawsel <- filter(raw, as.character(GNic_Type_fin) == "Financial Institution")
ICFI <- rawsel %>%
  select(starts_with("FIic_")) %>%
  select(-FIic_Percentage_green_investment) ## already in custG 4 all customers
ICFI <- cbind(rawsel[,1],ICFI)
colnames(ICFI) <- sub("FIic_", "", colnames(ICFI))
write.csv2(ICFI, fname_out_ICFI, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- PrivEq --------- READ PE-DATA OUT OF XLS INTO CSV -------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, sheet = "Input_Data_FMO_PE",
                 skip = 7)

## 2 - skip all calculated columns, just keep source-data
sel <- c(1, c(194:339))
raw <- select(raw, sel)

## 3 - rename column hdrs for easy processing
hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs[c(2:43)] <- paste0("PEc_", hdrs[c(2:43)])
pec <- hdrs[c(2:43)]
pec <- strsplit(pec, split = "__")
pec <- lapply(pec, "[", 1)
hdrs[c(2:43)] <- unlist(pec)
rm(pec)
per <- hdrs[c(45:69)]
n <- c(1:length(per))
per <- paste0("PEr_inv_sector",n)
hdrs[c(45:69)] <- per
per <- hdrs[c(71:95)]
n <- c(1:length(per))
per <- paste0("PEr_inv_amount",n)
hdrs[c(71:95)] <- per
per <- hdrs[c(97:121)]
n <- c(1:length(per))
per <- paste0("PEr_inv_country",n)
hdrs[c(97:121)] <- per
per <- hdrs[c(123:147)]
n <- c(1:length(per))
per <- paste0("PEr_inv_SME",n)
hdrs[c(123:147)] <- per
rm(per)
colnames(raw) <- hdrs
rm(hdrs)

## 4 - remove non-data columns
raw <- select(raw, -starts_with("X__"))

## 5 - skip all rows without a customer_id
raw <- filter(raw, !is.na(Customer_ID))

## 6 - split file into private eq. impact card and PEF-investments

## generate private equity impact card (impact at contract)
rawsel <- raw %>%
  select(starts_with("PEc_"))
ICPE <- cbind(raw[,1], rawsel)
colnames(ICPE) <- sub("PEc_", "", colnames(ICPE))
write.csv2(ICPE, fname_out_ICPE, row.names = FALSE,
           fileEncoding = "UTF-8")

## generate private equity fund investments (impact at review)
rawsel <- raw %>%
  select(starts_with("PEr_"))
rawsel <- cbind(raw[,1], rawsel)
colnames(rawsel) <- sub("PEr_", "", colnames(rawsel))
ix_sector <- c(2:26)
ix_amount <- c(27:51)
ix_country <- c(52:76)
ix_SME <- c(77:101)
k <- 1
PEFInv <- data.frame(Customer_ID = character(0),
                     sector = character(0),
                     amount = character(0),
                     country = character(0),
                     sme = character(0),
                     stringsAsFactors = FALSE)
for (i in seq_along(rawsel$Customer_ID)) {
  for (j in c(1:25)) {
    if(!is.na(rawsel[i,ix_amount[j]])) {
      PEFInv[k,] <- NA
      PEFInv[k,1] <- rawsel[i,1]
      PEFInv[k,2] <- rawsel[i,ix_sector[j]]
      PEFInv[k,3] <- rawsel[i,ix_amount[j]]
      PEFInv[k,4] <- rawsel[i,ix_country[j]]
      PEFInv[k,5] <- rawsel[i,ix_SME[j]]
      k <- k + 1
    }
  }
}
PEFInv$amount <- as.numeric(PEFInv$amount)

write.csv2(PEFInv, fname_out_PEFInv, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- EcEF ---- Economic Emission Factors for Direct Investment ---------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Economic_Input!C10:S33")

## 2 - rename column hdrs for easy processing
hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("_&_", "_", hdrs)
hdrs[1] <- "Region"
colnames(raw) <- hdrs
rm(hdrs)

## 4 - classify variables
raw$Region <- as.factor(raw$Region)

## 6 - wegschrijven CSV-file in datalake/clean_data
EcEF <- raw
write.csv2(EcEF, fname_out_EcEF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- CpIF ---- Capital Intensity Factors per region --------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Economic_Input!C38:S61")

## 2 - rename column hdrs for easy processing
hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("_&_", "_", hdrs)
hdrs[1] <- "Region"
colnames(raw) <- hdrs
rm(hdrs)

## 4 - classify variables
raw$Region <- as.factor(raw$Region)

## 6 - wegschrijven CSV-file in datalake/clean_data
CpIF <- raw
write.csv2(CpIF, fname_out_CpIF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- CBBD ---- Central Bank Breakdown per region -----------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Economic_Input!C66:S89")

## 2 - rename column hdrs for easy processing
hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("_&_", "_", hdrs)
hdrs[1] <- "Region"
colnames(raw) <- hdrs
rm(hdrs)

## 4 - classify variables
raw$Region <- as.factor(raw$Region)

## 6 - wegschrijven CSV-file in datalake/clean_data
CBBD <- raw
write.csv2(CBBD, fname_out_CBBD, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- OutElc ---- Output to Electricity per region ----------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Economic_Input!C94:S117")

## 2 - rename column hdrs for easy processing
hdrs <- colnames(raw)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("_&_", "_", hdrs)
hdrs[1] <- "Region"
colnames(raw) <- hdrs
rm(hdrs)

## 4 - classify variables
raw$Region <- as.factor(raw$Region)

## 6 - wegschrijven CSV-file in datalake/clean_data
OutElc <- raw
write.csv2(OutElc, fname_out_OutElc, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- Countries ---- Countries/Regions LUT     --------------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Lists!H5:I92")

## 2 - rename column hdrs for easy processing
colnames(raw)[1] <- "Country"
colnames(raw)[2] <- "Model_Region"

## 4 - classify variables
raw$Model_Region <- as.factor(raw$Model_Region)

## 6 - wegschrijven CSV-file in datalake/clean_data
Map_CntrReg <- raw
write.csv2(Map_CntrReg, fname_out_Map_CntrReg, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- Sectors ---- Sectors/Modeled-sectors LUT --------------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Lists!B5:C26")

## 2 - rename column hdrs for easy processing
colnames(raw)[1] <- "Sector"
colnames(raw)[2] <- "Sector_modeled"

## 4 - classify variables
raw$Sector_modeled <- as.factor(raw$Sector_modeled)

## 6 - wegschrijven CSV-file in datalake/clean_data
Map_SctrMdl <- raw
write.csv2(Map_SctrMdl, fname_out_Map_SctrMdl, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- IC-Sectors ---- IC-Sectors/Modeled-sectors LUT --------------------
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand uit de url, in het geheugen
raw <- read_xlsx(fname_in_raw, range = "Lists!E5:F23")

## 2 - rename column hdrs for easy processing
colnames(raw)[1] <- "Sector"
colnames(raw)[2] <- "Sector_modeled"

## 4 - classify variables
raw$Sector_modeled <- as.factor(raw$Sector_modeled)

## 6 - wegschrijven CSV-file in datalake/clean_data
Map_ICSctrMdl <- raw
write.csv2(Map_ICSctrMdl, fname_out_Map_ICSctrMdl, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
