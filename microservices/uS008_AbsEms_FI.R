## ----------------------------------------------------------------------- 
## -- uS008_AbsEms_FI -------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Calculates absolute emissions for financial institutions (scope 1 and 2)
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
## -- Input : customers                        (18 columns)             -- 
## --       : ecfactors                        (23   / 19 )             -- 
## --       : pkfactors                        (23   / 19 )             -- 
## --       : impactcard                       (101  / 23 )             -- 
## --       : sectormap                        (18   / 2  )             -- 
## --       : costPK_msme                      (parameter )             -- 
## --       : costPK_corp                      (parameter )             -- 
## -- Output: AE_FI                            (4  columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS008_AbsEms_FI <- function(customers, ecfactors, pkfactors, impactcard, 
                            costPK_msme, costPK_corp,
                            countrymap, sectormap) {

  require(tidyverse)
  ## 1 - Read in parameters -------------------------------------------------
  test <- FALSE
  if(!test) {
    custG <- customers
    EcEF <- ecfactors
    CpIF <- pkfactors
    ICFI <- impactcard
    lutICS <- sectormap
    lutC <- countrymap
  } 
  if(test) {
    custG <- read.csv2(fname_in_custG, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8")
    custG <- custG[custG$IC_type == "financial institution",]
    EcEF <- read.csv2(fname_in_EcEF1,
                          stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    CpIF <- read.csv2(fname_in_CpIF,
                         stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    ICFI <- read.csv2(fname_in_ICFI,
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    costPK_corp <- 0.73
    costPK_msme <- 1.2
  }

  ## 2 - prepare vector with GHG_sectors ---------------------
  GHG_Sectors <- gsub("_"," ", colnames(EcEF))[-1]
  GHG_Sectors[2] <- "Business Services"
  GHG_Sectors[3] <- "Cement & steel manufacturing"
  GHG_Sectors[11] <- "Mining & Quarrying"
  GHG_Sectors[17] <- "50% business services 50% construction"
  GHG_Sectors[18] <- "Central Bank Breakdown"

  V0s <- colnames(ICFI)[6:23]
  V0s <- gsub("_"," ", colnames(ICFI)[6:23])
  V0s[7] <- "Heavy industry"
  V0m <- vector(mode = "character", 
                   length = length(V0s))
  for (i in seq_along(V0s)) {
    V0m[i] <- lutICS$Sector_modeled[lutICS$Sector == V0s[i]]
  }

  ## 3 - setup resulting dataframe ------------------------------------------
  dfAE <- data.frame(Customer_ID = character(0),
                     abs_ems = numeric(0),
                     method = character(0),
                     stringsAsFactors = FALSE)
  
  ## 3 - absolute emissions (always modeled) --------------------------------
  ## Sectors in EcEF, ICFI are GHG Sectors
  ## Sectors in the output need to be IC-Modeled Sectors !
  custG <- arrange(custG, desc(Customer_ID))
  for(i in seq_along(custG$Customer_ID)) {
    dfAE[i,] <- NA
    curcust <- custG$Customer_ID[i]
    dfAE$Customer_ID[i] <- curcust
    curcountry <- custG$Country[i]
    curcountry <- lutC$Model_Region[lutC$Country == curcountry]
    V2 <- EcEF[EcEF$GHG_country == curcountry,-1]
    V3 <- CpIF[CpIF$GHG_country == curcountry,-1]
    V4 <- ICFI[ICFI$Customer_ID == curcust,]
    V1 <- V4[,c(6:23)]
    PrcGI <- custG$IC_prcGreenInv[i]/100
    NetPort <- custG$Net_portfolio[i]
    noNP <- is.na(NetPort)
    nocando <- noNP | NetPort == 0
    if(!nocando) {
      SecEms <- vector(mode = "numeric", 
                         length = length(V0m))
      for(j in c(1:length(V0m))) {
        cursector <- V0m[j]
        SecEms[j] <- NA
        if(!noNP & !is.na(as.numeric(V1[j]))) {
          a <- as.numeric(V2[which(GHG_Sectors %in% cursector)])
          b <- as.numeric(V3[which(GHG_Sectors %in% cursector)])
          msme <- sum(V4$Volume_of_microfinance,
                      V4$Volume_of_SME_portfolio, na.rm = TRUE)
          c <- ifelse(is.na(msme), 1, 
                      (msme*costPK_msme + (1-msme)*costPK_corp))
          d <- as.numeric(V1[j])
          SecEms[j] <- a * b * c * d * NetPort / 10^6
          }
        }  
      AbsEms <- sum(SecEms, na.rm = TRUE)
      AbsEms <- ifelse(AbsEms < 0, 0, AbsEms)
      dfAE$abs_ems[i] <- AbsEms * (1 - 0.2 * PrcGI)
      dfAE$method[i] <- "modeled"
      }
    } 

  ## 6 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(dfAE, desc(Customer_ID))
  return(Out)
}