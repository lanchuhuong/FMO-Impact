## Script for reading Input-datasets from Excel-sheet
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This temporary script reads Excel-sheet: Absolute GHG Accounting.xlsx
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
## -- Input : Absolute GHG Accounting.xlsx    / Input_Data_FMO          -- 
## -- LUT's :                                                           -- 
## -- Output: Cust_General.csv                  (247    / 18)           -- 
## --       : IC_Corporate.csv                  (48     / 7 )           -- 
## --       : IC_ProjFin.csv                    (59     / 6 )           -- 
## --       : IC_FinInst.csv                    (101    / 23)           -- 
## --       : IC_PrivEq.csv                     (39     / 43)           -- 
## --       : PEF_Investments.csv               (117    / 5 )           -- 
## --       : PEF_RegionPerc.csv                (18     / 3 )           -- 
## --       : Fctr_EcEmis1.csv                  (23     / 19)           -- 
## --       : Fctr_EcEmis2.csv                  (23     / 19)           -- 
## --       : Fctr_CapInt.csv                   (23     / 19)           -- 
## --       : Fctr_CBbrkdwn.csv                 (23     / 17)           -- 
## --       : Fctr_Out2Elec.csv                 (23     / 17)           -- 
## --       : MAP_CntrReg.csv                   (87     / 2 )           -- 
## --       : MAP_ICSctrMdl.csv                 (18     / 2 )           -- 
## --       : MAP_SctrMdl.csv                   (21     / 2 )           -- 
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
indate = "2019-04-11"
lutdate = ""
outdate = "2019-04-11"

## Setup filenames
fname_in_raw <- paste0(rawdir,"Absolute GHG Accounting", "_", indate,".xlsx")
fname_out_custG <- paste0(clndir,"Cust_General", "_", outdate,".csv")
fname_out_ICCp <- paste0(clndir,"IC_Corporate", "_", outdate,".csv")
fname_out_ICPF <- paste0(clndir,"IC_ProjFin", "_", outdate,".csv")
fname_out_ICFI <- paste0(clndir,"IC_FinInst", "_", outdate,".csv")
fname_out_ICPE <- paste0(clndir,"IC_PrivEq", "_", outdate,".csv")
fname_out_PEFInv <- paste0(clndir,"PEF_Investments", "_", outdate,".csv")
fname_out_PEFReg <- paste0(clndir,"PEF_RegionPerc", "_", outdate,".csv")
fname_out_EcEF1 <- paste0(clndir,"Fctr_EcEmis1", "_", outdate,".csv")
fname_out_EcEF2 <- paste0(clndir,"Fctr_EcEmis2", "_", outdate,".csv")
fname_out_CpIF <- paste0(clndir,"Fctr_CapInt", "_", outdate,".csv")
fname_out_CBBD <- paste0(clndir,"Fctr_CBbrkdwn", "_", outdate,".csv")
fname_out_OutElc <- paste0(clndir,"Fctr_Out2Elec", "_", outdate,".csv")
fname_out_Map_CntrReg <- paste0(lutdir,"Map_CntrReg", "_", outdate,".csv")
fname_out_Map_SctrMdl <- paste0(lutdir,"Map_SctrMdl", "_", outdate,".csv")
fname_out_Map_ICSctrMdl <- paste0(lutdir,"Map_ICSctrMdl", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- raw ------------ READ ACBS/FIA-DATA OUT OF XLS INTO CSV -----------
## ----------------------------------------------------------------------- 
## 1 - read raw xls.file
raw <- read_xlsx(fname_in_raw, sheet = "Input_Data_FMO",
                 skip = 6)
## 2 - skip all empty columns (named: X__) and empty observation (units)
raw <- select(raw, -starts_with("X__"))
raw <- raw[-1,]
raw <- raw[!is.na(raw$`Customer ID`),]

## ----------------------------------------------------------------------- 
## --- custG ---------- Construct general customer data ------------------
## ----------------------------------------------------------------------- 
## 1 - start with columns that contain general customer data
rawsel <- raw[,c(1:9)]
hdrs <- colnames(rawsel)
hdrs <- gsub("-", "_", hdrs)
hdrs <- gsub(" ", "_", hdrs)
hdrs <- gsub("___", "_", hdrs)
colnames(rawsel) <- hdrs
rm(hdrs)

## 2 - Split up Impact_Card_Type into sector/IC-type
pattern <- "\xE2\x80\x93"
rawsel <- separate(rawsel, Impact_Card_Type, 
                 into = c("Strat_Sector", "IC_Type"),
                 sep = pattern, fill = "left")
rawsel$IC_Type <- sub(" corporate", "corporate", rawsel$IC_Type)
rawsel$IC_Type <- sub(" project finance", "project finance", rawsel$IC_Type)
rawsel$IC_Type <- tolower(rawsel$IC_Type)
rawsel$Strat_Sector <- sub(" $", "", rawsel$Strat_Sector)
rawsel$IC_Type[is.na(rawsel$IC_Type)] <- "unknown"
rawsel$Strat_Sector[is.na(rawsel$Strat_Sector)] <- "unknown"

## 3 - Write file
custG <- rawsel
write.csv2(custG, fname_out_custG, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)
table(custG$IC_Type)

## ----------------------------------------------------------------------- 
## --- ICCp ---------- Construct ImpactCard Corporate --------------------
## ----------------------------------------------------------------------- 
## 1 - get rows for corporates and select columns that contain corporate data
ix <- which(custG$IC_Type=="corporate")
rawsel <- raw[ix,]
rawsel <- rawsel[,c(1, 10:15)]

## 2 - cleanup columns
colnames(rawsel) <- c("Customer_ID", "Non_current_assets", "Revenues",
                      "GHG_Scope_not_specified", "GHG_Scope1",
                      "GHG_Scope2", "GHG_Scope3")

## 3 - Write file
ICCp <- rawsel
write.csv2(ICCp, fname_out_ICCp, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)

## ----------------------------------------------------------------------- 
## --- ICPF ---------- Construct ImpactCard Project-Finance --------------
## ----------------------------------------------------------------------- 
## 1 - get rows for project finance / select columns that contain PrFin data
ix <- which(custG$IC_Type=="project finance")
rawsel <- raw[ix,]
rawsel <- rawsel[,c(1, 16:20)]

## 2 - cleanup columns
colnames(rawsel) <- c("Customer_ID", "Project_size",
                      "GHG_Scope_not_specified", "GHG_Scope1",
                      "GHG_Scope2", "GHG_Scope3")

## 3 - Write file
ICPF <- rawsel
write.csv2(ICPF, fname_out_ICPF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)

## ----------------------------------------------------------------------- 
## --- ICFI ---------- Construct ImpactCard Financial Institutions -------
## ----------------------------------------------------------------------- 
## 1 - get rows for fin. inst's and select columns that contain FI data
ix <- which(custG$IC_Type=="financial institution")
rawsel <- raw[ix,]
rawsel <- rawsel[,c(1, 21:41)]

## 2 - cleanup columns
colnames(rawsel)[1:4] <- c("Customer_ID", "Total_assets",
                      "Share_micro_loans", "Share_SME_loans")

## 3 - Write file
ICFI <- rawsel
write.csv2(ICFI, fname_out_ICFI, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)

## ----------------------------------------------------------------------- 
## --- PrivEq --------- READ PE-DATA OUT OF XLS INTO CSV -------------------
## ----------------------------------------------------------------------- 
## 1 - read file and skip empty rows and calculated columns
raw <- read_xlsx(fname_in_raw, sheet = "Input_Data_FMO_PE",
                 skip = 7)
raw <- raw[-1,]
raw <- raw[!is.na(raw$`Customer ID`),]
raw <- raw[,c(2, c(238:383))]

## ----------------------------------------------------------------------- 
## --- ICPE ---------- Construct ImpactCard Private Equity Funds ---------
## ----------------------------------------------------------------------- 
## 1 - select columns that contain data for impact card PE
rawsel <- raw[,c(1:19, 41:43)]

## 2 - cleanup columns
colnames(rawsel)[1] <- "Customer_ID"
hdrs <- colnames(rawsel)[2:19]
hdrs <- strsplit(hdrs, split = "__")
hdrs <- lapply(hdrs, "[", 1)
colnames(rawsel)[2:19] <- unlist(hdrs)
colnames(rawsel)[20:22] <- c("MFI", "SME", "CORP")

## 3 - Write file
ICPE <- rawsel
write.csv2(ICPE, fname_out_ICPE, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)

## ----------------------------------------------------------------------- 
## --- PEFReg ------ Construct Perc.Assets per Region @ contract ---------
## ----------------------------------------------------------------------- 
## 1 - select columns that contain data for regions @ contract PE
rawsel <- raw[,c(1, 20:40)]

## 2 - cleanup columns
colnames(rawsel)[1] <- "Customer_ID"
hdrs <- colnames(rawsel)[2:22]
hdrs <- strsplit(hdrs, split = "__")
hdrs <- lapply(hdrs, "[", 1)
colnames(rawsel)[2:22] <- unlist(hdrs)
rm(hdrs)

## 3 - gather all 21 region-columns as values in one column "region",
##     while maintaining the asset-percentage as "Asset_perc"
PEFReg <- rawsel %>%
  gather(key = "AssetRegion", value = "AssetPerc", -Customer_ID)

## 4 - change 21 regions into GHG-Countries based on lut in XLS
lut <- read_xlsx(fname_in_raw, range = "Lists!S5:T26")
colnames(lut) <- c("In", "Out")
ix <- PEFReg$AssetRegion %in% lut$In
for (i in seq_along(PEFReg$Customer_ID)) {
  if(ix) {
  PEFReg$AssetRegion[i] <- lut$Out[lut$In == PEFReg$AssetRegion[i]]
  }
}

## 5 - Write file
write.csv2(PEFReg, fname_out_PEFReg, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(rawsel)
rm(lut)

## ----------------------------------------------------------------------- 
## --- PEFInv ------ Construct Perc.Investments per Region @ review ------
## ----------------------------------------------------------------------- 
## 1 - select columns that contain data for regions @ contract PE
rawsel <- raw[,c(1, 45:147)]
rawsel_sector <- rawsel[,c(1,2:26)]
rawsel_outstanding <- rawsel[,c(1,28:52)]
rawsel_country <- rawsel[,c(1,54:78)]
rawsel_sme <- rawsel[,c(1,80:104)]

## 2 - build df with private equity fund investments (impact at review)
##     k = PEFInv rows, i = customerId, j = column-counter (1:25)
PEFInv <- data.frame(Customer_ID = character(0),
                     sector = character(0),
                     outstanding = character(0),
                     country = character(0),
                     sme = character(0),
                     stringsAsFactors = FALSE)
k <- 1
for (i in seq_along(rawsel_outstanding$`Customer ID`)) {
  for (j in c(2:26)) {
    if(!is.na(rawsel_outstanding[i,j])) {
      PEFInv[k,] <- NA
      PEFInv[k,1] <- rawsel_country[i,1]
      PEFInv[k,2] <- rawsel_sector[i,j]
      PEFInv[k,3] <- rawsel_outstanding[i,j]
      PEFInv[k,4] <- rawsel_country[i,j]
      PEFInv[k,5] <- rawsel_sme[i,j]
      k <- k + 1
    }
  }
}
PEFInv$outstanding <- as.numeric(PEFInv$outstanding)

write.csv2(PEFInv, fname_out_PEFInv, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
rm(rawsel)
rm(rawsel_country)
rm(rawsel_outstanding)
rm(rawsel_sector)
rm(rawsel_sme)

## ----------------------------------------------------------------------- 
## --- CBBD ---- Central Bank Breakdown per region -----------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Input_Impact_Model!C65:S88")
colnames(raw)[1] <- "GHG_Country"
CBBD <- raw
write.csv2(CBBD, fname_out_CBBD, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- CpIF ---- Capital Intensity Factors per region --------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Input_Impact_Model!C37:S60")
colnames(raw)[1] <- "GHG_Country"

## 2 - add calculated columns
raw <- mutate(raw, `50% business services 50% construction` =
                (`Business services`/2 + Construction/2))
raw <- mutate(raw, `1/3 business services – 1/3 trade – 1/3 construction` =
                (`Business services`/3 + Trade/3 + Construction/3))
## CBBD = sumproduct(PKF * CBBD)
raw <- mutate(raw, `Central Bank Breakdown` = NA)
for(i in c(1:nrow(raw))) {
  cur <- raw$GHG_Country[i]
  v1 <- raw[i,2:17]
  j <- which(CBBD$GHG_Country %in% cur)
  v2 <- CBBD[j,2:17]
  raw$`Central Bank Breakdown`[i] <- sum(v1*v2)
}

## 4 - wegschrijven CSV-file in datalake/clean_data
CpIF <- raw
write.csv2(CpIF, fname_out_CpIF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
rm(v1, v2)

## ----------------------------------------------------------------------- 
## -- EcEF1 -- Economic Emission Factors for Direct Investment (Scope 1) -
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand (also uses CBBD)
raw <- read_xlsx(fname_in_raw, range = "Input_Impact_Model!C9:S32")
colnames(raw)[1] <- "GHG_Country"

## 2 - add calculated columns
raw <- mutate(raw, `50% business services 50% construction` =
                (`Business services`/2 + Construction/2))
raw <- mutate(raw, `1/3 business services – 1/3 trade – 1/3 construction` =
                (`Business services`/3 + Trade/3 + Construction/3))
## CBBD = sumproduct(EcEF1 * CBBD * PKF / sumproduct(CBBD * PKF))
raw <- mutate(raw, `Central Bank Breakdown` = NA)
for(i in c(1:nrow(raw))) {
  cur <- raw$GHG_Country[i]
  v1 <- raw[i,2:17]
  j <- which(CBBD$GHG_Country %in% cur)
  v2 <- CBBD[j,2:17]
  j <- which(CpIF$GHG_Country %in% cur)
  v3 <- CpIF[j,2:17]
  raw$`Central Bank Breakdown`[i] <- (sum(v1*v2*v3)/sum(v2*v3))
}

## 3 - wegschrijven CSV-file in datalake/clean_data
EcEF1 <- raw
write.csv2(EcEF1, fname_out_EcEF1, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
rm(v1, v2, v3)

## ----------------------------------------------------------------------- 
## --- OutElc ---- Output to Electricity per region ----------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Input_Impact_Model!C93:S116")
colnames(raw)[1] <- "GHG_Country"
OutElc <- raw
write.csv2(OutElc, fname_out_OutElc, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## -- EcEF2 -- Economic Emission Factors for Direct Investment (Scope 1) -
## ----------------------------------------------------------------------- 
## 1 - read data
## EcEF2 is based on Output of Electricity (OutElc) and emission factors 
## scope 1 (EcEF1), also the CBBD. No new data needed

## 2 - calculate entries for country/sector
raw <- EcEF1
raw[,2:19] <- 0
for(i in seq_along(raw$GHG_Country)){
  raw[i,2:17] <- OutElc[i,2:17] * EcEF1$Electricity[i]
}

## 3 - add two extra columns (50BS/50C and CB-Breakdown)
raw <- mutate(raw, `50% business services 50% construction` =
                (`Business services`/2 + Construction/2))
raw <- mutate(raw, `1/3 business services – 1/3 trade – 1/3 construction` =
                (`Business services`/3 + Trade/3 + Construction/3))
## CBBD = sumproduct(EcEF2 * CBBD * PKF / sumproduct(CBBD * PKF))
raw <- mutate(raw, `Central Bank Breakdown` = NA)
for(i in c(1:nrow(raw))) {
  cur <- raw$GHG_Country[i]
  v1 <- raw[i,2:17]
  j <- which(CBBD$GHG_Country %in% cur)
  v2 <- CBBD[j,2:17]
  j <- which(CpIF$GHG_Country %in% cur)
  v3 <- CpIF[j,2:17]
  raw$`Central Bank Breakdown`[i] <- (sum(v1*v2*v3)/sum(v2*v3))
}

## 4 - wegschrijven CSV-file in datalake/clean_data
EcEF2 <- raw
write.csv2(EcEF2, fname_out_EcEF2, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
rm(v1, v2, v3)

## ----------------------------------------------------------------------- 
## --- Countries ---- Countries/Regions LUT     --------------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Lists!H5:I132")
colnames(raw)[1] <- "Country"
colnames(raw)[2] <- "Model_Region"
Map_CntrReg <- raw
write.csv2(Map_CntrReg, fname_out_Map_CntrReg, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- Sectors ---- Sectors/Modeled-sectors LUT --------------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Lists!B5:C26")
colnames(raw)[1] <- "Sector"
colnames(raw)[2] <- "Sector_modeled"
Map_SctrMdl <- raw
write.csv2(Map_SctrMdl, fname_out_Map_SctrMdl, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

## ----------------------------------------------------------------------- 
## --- IC-Sectors ---- IC-Sectors/Modeled-sectors LUT --------------------
## ----------------------------------------------------------------------- 
raw <- read_xlsx(fname_in_raw, range = "Lists!E5:F23")
colnames(raw)[1] <- "Sector"
colnames(raw)[2] <- "Sector_modeled"
Map_ICSctrMdl <- raw
write.csv2(Map_ICSctrMdl, fname_out_Map_ICSctrMdl, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)
