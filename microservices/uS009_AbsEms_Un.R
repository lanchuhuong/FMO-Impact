## ----------------------------------------------------------------------- 
## -- uS009_AbsEms_Un -------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Calculates absolute emissions for clients of unknown type (scope 1 and 2)
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This function calculates investment percentage per Size (SME vs. Corp)
## for each FI-customer. First it tries to find actual investments (at review)
## as present in "Investments". If no actual investments are made
## than we try to use the indicated sectors at the time of contracting
## as present in "Contracted".
##
## INPUT PARAMETERS / OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : customers                        (10 columns)             -- 
## --       : ecfactors                        (23   / 20 )             -- 
## --       : pkfactors                        (23   / 20 )             -- 
## --       : countrymap                       (127  / 2  )             -- 
## --       : sectormap                        (21   / 2  )             -- 
## -- Output: AE_FI                            (4  columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS009_AbsEms_Un <- function(customers, ecfactors, pkfactors,
                            countrymap, sectormap) {

  require(tidyverse)
  ## 1 - Read in parameters -------------------------------------------------
  custG <- customers
  EcEF <- ecfactors
  CpIF <- pkfactors
  lutS <- sectormap
  lutC <- countrymap

  ## 2 - prepare vector with GHG_sectors ---------------------
  GHG_Sectors <- colnames(EcEF)[-1]

  ## 3 - setup resulting dataframe ------------------------------------------
  dfAE <- data.frame(Customer_ID = character(0),
                     abs_ems = numeric(0),
                     method = character(0),
                     stringsAsFactors = FALSE)
  
  ## 4 - absolute emissions (always no data) --------------------------------
  custG <- arrange(custG, desc(Customer_ID))
  for(i in seq_along(custG$Customer_ID)) {
    dfAE[i,] <- NA
    curcust <- custG$Customer_ID[i]
    dfAE$Customer_ID[i] <- curcust
    NetPort <- custG$Net_portfolio[i]
    NetPort <- ifelse(is.na(NetPort), 0, NetPort)
    curcountry <- custG$Country[i]
    cursector <- custG$Client_sector[i]
    GHG_Country <- lutC$Model_Region[tolower(lutC$Country) == tolower(curcountry)]
    GHG_Sector <- lutS$Sector_modeled[tolower(lutS$Sector) == tolower(cursector)]
    SctIx <- as.numeric(which(colnames(EcEF) %in% GHG_Sector))
    AbsEms <- (EcEF[which(EcEF$GHG_Country == GHG_Country), SctIx] *
                 CpIF[which(CpIF$GHG_Country == GHG_Country), SctIx]*
                 custG$Net_portfolio[i] / 10^6)
    AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
    dfAE$method[dfAE$Customer_ID == curcust & !is.na(AbsEms)] <- "nodata"
    dfAE$abs_ems[dfAE$Customer_ID == curcust] <- AbsEms
  } 

  ## 5 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(dfAE, desc(Customer_ID))
  return(Out)
}