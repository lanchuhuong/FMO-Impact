## ----------------------------------------------------------------------- 
## -- uS006_AbsEms_PrFin -------------------------------------------------- 
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
## --       : scope                            (parameter )             -- 
## --       : countrymap                       (2  columns)             -- 
## --       : sectormap                        (2  columns)             -- 
## -- Output: AE-corp                          (4  columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS006_AbsEms_PrFin <- function(impactcard, customers, factors, scope,
                               countrymap, sectormap) {
  require(tidyverse)
  ## 1 - Read in parameters -------------------------------------------------
  ICPF <- impactcard
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
    if(scope == 1) {
      nocando <- is.na(ICPF$GHG_Scope1[ICPF$Customer_ID == curcust]) |
        is.na(ICPF$Project_size[ICPF$Customer_ID == curcust])
      if(!nocando){
        AbsEms <- (ICPF$GHG_Scope1[ICPF$Customer_ID == curcust] *
                  max(custG$Net_portfolio[i],0) /
                  ICPF$Project_size[ICPF$Customer_ID == curcust])
        AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
        dfAE$method[i] <- "primary"
        dfAE$abs_ems[i] <- AbsEms
      }
    }
    if(scope == 2) {
      ICPF$GHG_Scope2 <- as.numeric(ICPF$GHG_Scope2)
      nocando <- is.na(ICPF$GHG_Scope2[ICPF$Customer_ID == curcust]) |
        is.na(ICPF$Project_size[ICPF$Customer_ID == curcust])
      if(!nocando){
        AbsEms <- (ICPF$GHG_Scope2[ICPF$Customer_ID == curcust] *
                     max(custG$Net_portfolio[i],0) /
                     ICPF$Project_size[ICPF$Customer_ID == curcust])
        AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
        dfAE$method[i] <- "primary"
        dfAE$abs_ems[i] <- AbsEms
      }
    }
  } 
  
  ## 4 - calculate modeled absolute emissions if primary not possible ---------
  for(i in seq_along(custG$Customer_ID)){
    curcust <- custG$Customer_ID[i]
    curcountry <- custG$Country[i]
    cursector <- custG$Client_sector[i]
    GHG_Country <- lutC$Model_Region[toupper(lutC$Country) == curcountry]
    GHG_Sector <- lutS$Sector_modeled[tolower(lutS$Sector) == tolower(cursector)]
    SctIx <- as.numeric(which(colnames(EcEF) %in% GHG_Sector))
    nocando <- (is.na(custG$Net_portfolio[i]) |
                  !is.na(dfAE$method[dfAE$Customer_ID == curcust]))
    if(!nocando){
      AbsEms <- (EcEF[which(EcEF$GHG_Country == GHG_Country), SctIx] *
                   custG$Net_portfolio[i] /
                   10^6)
      AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
      dfAE$method[dfAE$Customer_ID == curcust] <- "nodata"
      dfAE$abs_ems[dfAE$Customer_ID == curcust] <- AbsEms
    }
  } 
  
  ## 6 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(dfAE, desc(Customer_ID))
  return(Out)
}