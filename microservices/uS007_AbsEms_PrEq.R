## ----------------------------------------------------------------------- 
## -- uS007_AbsEms_PrEq -------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Calculates absolute emissions for private equity funds (scope 1 and 2)
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
  test <- FALSE
  if(!test) {
    custG <- customers
    PEFConsECF <- ecfactors
    PEFConsPKF <- pkfactors
    PEFConsSize <- sizes
    PEFConsSctrs <- sectors
    lutICS <- sectormap
  } 
  if(test) {
    custG <- read.csv2(fname_in_custG, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8")
    custG <- custG[custG$IC_type == "private equity fund",]
    PEFConsECF <- read.csv2(fname_in_PEFConsEC1fctrs,
                          stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    PEFConsPKF <- read.csv2(fname_in_PEFConsPKfctrs,
                         stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    PEFConsSize <- read.csv2(fname_in_PEFConsSize,
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    PEFConsSctrs <- read.csv2(fname_in_PEFConsSctrs,
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    costPK_corp <- 0.73
    costPK_msme <- 1.2
  }

  ## 2 - prepare vector with GHG_sectors ---------------------
  GHG_Sectors <- gsub("_"," ", colnames(PEFConsECF))[-1]
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
  ## Sectors in PEFConsSctrs are Impact Card Sectors
  ## Sectors in PEF_ConsECF are GHG Sectors
  ## Sectors in the output need to be IC-Modeled Sectors !
  ## Please also note the check on V2Agriculture, Ref Xls-tab Calcs!AT219
  ##               remove if not needed after check
  for(i in seq_along(custG$Customer_ID)) {
    dfAE[i,] <- NA
    curcust <- custG$Customer_ID[i]
    dfAE$Customer_ID[i] <- curcust
    V1 <- PEFConsSctrs[PEFConsSctrs$Customer_ID == curcust,-1]
    V2 <- PEFConsECF[PEFConsECF$Customer_ID == curcust,-1]
    V3 <- PEFConsPKF[PEFConsPKF$Customer_ID == curcust,-1]
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
        if(!noNP & !is.na(as.numeric(V1[j]))) {
          a <- as.numeric(V2[which(GHG_Sectors %in% cursector)])
          b <- as.numeric(V3[which(GHG_Sectors %in% cursector)])
          c <- ifelse(is.na(V4$msme), 1, 
                      (V4$msme*costPK_msme + V4$corporate*costPK_corp))
          d <- as.numeric(V1[j])
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