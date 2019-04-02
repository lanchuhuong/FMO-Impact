## ----------------------------------------------------------------------- 
## -- uS007_AbsEms_PrEq -------------------------------------------------- 
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
## -- Input : customers                        (18 columns)             -- 
## --       : ecfactors                        (39   / 19 )             -- 
## --       : pkfactors                        (39   / 19 )             -- 
## --       : sizes                            (39   / 4  )             -- 
## --       : sectors                          (252  / 4  )             -- 
## --       : sectormap                        (18   / 2  )             -- 
## --       : costPK_msme                      (parameter )             -- 
## --       : costPK_corp                      (parameter )             -- 
## -- Output: AE_PrEq                          (4  columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS007_AbsEms_PrEq <- function(customers, ecfactors, pkfactors,
                               sizes, sectors, costPK_msme,
                               costPK_corp, sectormap) {
  require(tidyverse)
  ## 1 - Read in parameters -------------------------------------------------
  custG <- customers
  PEF_ConsECfctrs <- ecfactors
  PEF_ConsPKfctrs <- pkfactors
  PEF_ConsSize <- sizes
  PEF_ConsSctrs <- sectors
  # PEF_ConsSize <- PEFConsSize
  # PEF_ConsSctrs <- PEFConsSctrs
  # PEF_ConsECfctrs <- PEF_ConsEC1fctrs
  # PEF_ConsPKfctrs <- PEF_ConsPKfctrs
  lutICS <- sectormap
  
  ## 2 - prepare data frame PEFConsSctrs & GHG_sectors ---------------------
  PEFConsSctrs$sector <- gsub("_", " ", PEFConsSctrs$sector)
  PEFConsSctrs <- select(PEFConsSctrs, -status)
  PEFConsSctrs2 <- spread(PEFConsSctrs, key = sector, value = perc)
  PEFConsSctrs2 <- select(PEFConsSctrs2, 
                          c(1:6,16,7,11,12,8,10,15,17,18,19,9,14,13))
  GHG_Sectors <- gsub("_"," ", colnames(PEF_ConsECfctrs))[-1]
  GHG_Sectors[2] <- "Business Services"
  GHG_Sectors[3] <- "Cement & steel manufacturing"
  GHG_Sectors[11] <- "Mining & Quarrying"
  GHG_Sectors[17] <- "50% business services 50% construction"
  GHG_Sectors[18] <- "Central Bank Breakdown"

  ## 3 - setup resulting dataframe ------------------------------------------
  dfAE <- data.frame(Customer_ID = character(0),
                     abs_ems = numeric(0),
                     method = character(0),
                     stringsAsFactors = FALSE)
  
  ## 3 - absolute emissions (always modeled) --------------------------------
  ## custG <- bck
  ## bck <- custG
  ## custG <- custG[custG$Customer_ID == "C10003152",]
  for(i in seq_along(custG$Customer_ID)) {
    dfAE[i,] <- NA
    curcust <- custG$Customer_ID[i]
    dfAE$Customer_ID[i] <- curcust
    V1 <- PEFConsSctrs2[PEFConsSctrs2$Customer_ID == curcust,]
    V2 <- PEF_ConsECfctrs[PEF_ConsECfctrs$Customer_ID == curcust,]
    V3 <- PEF_ConsPKfctrs[PEF_ConsPKfctrs$Customer_ID == curcust,]
    V4 <- PEFConsSize[PEFConsSize$Customer_ID == curcust,]
    NetPort <- custG$Net_portfolio[i]
    noNP <- is.na(NetPort)
    nocando <- is.na(V2$Agriculture) |
              (sum(V2[-1],na.rm = TRUE) == 0 &
              NetPort != 0)
    if(!nocando) {
      SecEms <- vector(mode = "numeric", 
                         length = length(lutICS$Sector_modeled))
      for(j in c(1:length(lutICS$Sector_modeled))) {
        cursector <- lutICS$Sector_modeled[j]
        SecEms[j] <- NA
        if(!noNP & !is.na(as.numeric(V1[j+1]))) {
          a <- as.numeric(V2[which(GHG_Sectors %in% cursector)+1])
          b <- as.numeric(V3[which(GHG_Sectors %in% cursector)+1])
          c <- ifelse(is.na(V4$msme), 1, 
                      (V4$msme*costPK_msme + V4$corporate*costPK_corp))
          d <- as.numeric(V1[j+1])
          SecEms[j] <- a * b * c * d * NetPort / 10^6
          }
        }  
      AbsEms <- sum(SecEms, na.rm = TRUE)
      dfAE$abs_ems[i] <- ifelse(AbsEms < 0, 0, AbsEms)
      dfAE$method[i] <- "modeled"
      }
    } 

  ## 6 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(dfAE, desc(Customer_ID))
  return(Out)
}