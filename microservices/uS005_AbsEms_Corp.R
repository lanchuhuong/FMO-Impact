## ----------------------------------------------------------------------- 
## -- uS005_AbsEms_Corp -------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Calculates absolute emissions for corporate customer (scope 1 and 2)
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This function calculates investment percentage per Size (SME vs. Corp)
## for each PEF-customer. First it tries to find actual investments (at review)
## as present in "Investments". If no actual investments are made
## than we try to use the indicated sectors at the time of contracting
## as present in "Contracted".
##
## INPUT PARAMETERS / OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : impactcard                       (7  columns)             -- 
## --       : customers                        (18 columns)             -- 
## --       : factors                          (19 columns)             -- 
## --       : countrymap                       (2  columns)             -- 
## --       : sectormap                        (2  columns)             -- 
## -- Output: AE-corp                          (4  columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS005_AbsEms_Corp <- function(impactcard, customers, factors,
                              countrymap, sectormap) {
  require(tidyverse)
  ## 1 - Read in parameters -------------------------------------------------
  ICCp <- impactcard
  custG <- customers
  EcEF <- factors
  lutC <- countrymap
  lutS <- sectormap
  
  ## 2 - setup resulting dataframe ------------------------------------------
  dfAE <- data.frame(Customer_ID = character(0),
                     abs_ems = numeric(0),
                     method = character(0),
                     stringsAsFactors = FALSE)
  
  ## 3 - absolute emissions (primary) ---------------------------------------
  for(i in seq_along(custG$Customer_ID)){
    dfAE[i,] <- NA
    curcust <- custG$Customer_ID[i]
    dfAE$Customer_ID[i] <- curcust
    nocando <- is.na(ICCp$GHG_Scope_1[ICCp$Customer_ID == curcust]) |
      is.na(ICCp$Non_current_assets[ICCp$Customer_ID == curcust])
    if(!nocando){
      AbsEms <- (ICCp$GHG_Scope_1[ICCp$Customer_ID == curcust] *
      custG$Net_portfolio[i] /
      ICCp$Non_current_assets[ICCp$Customer_ID == curcust])
      AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
      dfAE$method[i] <- "primary"
      dfAE$abs_ems[i] <- AbsEms
      }
  } 

  ## 3 - get list of comparable EcEF-columnheaders (temporary) ---------------
  sectorheaders <- gsub("_"," ", colnames(EcEF))
  sectorheaders[12] <- "Mining & Quarrying"
  sectorheaders[18] <- "50% business services 50% construction"
  sectorheaders[19] <- "Central Bank Breakdown"
  
  ## 4 - temp-fix in Client-sectors in custG (ACBS/FIA) ----------------------
  custG$Client_sector <- gsub("Agri Pro","Agri pro", custG$Client_sector)

  ## 5 - calculate modeled absolute emissions if primary not possible ---------
  for(i in seq_along(custG$Customer_ID)){
    curcust <- custG$Customer_ID[i]
    nocando <- (is.na(custG$Net_portfolio[i]) |
      is.na(ICCp$Non_current_assets[ICCp$Customer_ID == curcust]) |
      !is.na(dfAE$method[dfAE$Customer_ID == curcust]))
    if(!nocando){
      GHG_Country <- lutC$Model_Region[lutC$Country == custG$Country[i]]
      GHG_Sector <- lutS$Sector_modeled[lutS$Sector == custG$Client_sector[i]]
      SctIx <- as.numeric(which(sectorheaders %in% GHG_Sector))
      AbsEms <- (EcEF[which(EcEF$GHG_country == GHG_Country), SctIx] *
                   ICCp$Revenues[ICCp$Customer_ID == curcust] /
                   ICCp$Non_current_assets[ICCp$Customer_ID == curcust] *
                   custG$Net_portfolio[i] /
                   10^6)
      AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
      dfAE$method[dfAE$Customer_ID == curcust] <- "modeled"
      dfAE$abs_ems[dfAE$Customer_ID == curcust] <- AbsEms
    }
  } 

  ## 6 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(dfAE, desc(Customer_ID))
  return(Out)
}