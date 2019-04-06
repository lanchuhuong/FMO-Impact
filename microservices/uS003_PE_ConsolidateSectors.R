## ----------------------------------------------------------------------- 
## -- uS003_PE_ConsolidateSectors ---------------------------------------- 
## ----------------------------------------------------------------------- 
## Generates investment percentage per IC-sector for PEF-customers
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This function calculates investment percentage per Impact Card Sector
## for each PEF-customer. First it tries to find actual investments (at review)
## as present in "Investments". If no actual investments are made
## than we try to use the indicated sectors at the time of contracting
## as present in "Contracted".
##
## INPUT PARAMETERS / OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : Investments                       (5 columns)             -- 
## --       : Contracted                        (3 columns)             -- 
## -- LUT   : Map_ICSctrMdl                     (2 columns)             -- 
## -- Output: PE_InvPctPerSector                (4 columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS003_PE_ConsolidateSectors <- function(investments, contracted) {
  require(tidyverse)
  ## 1 - Read in parameters: investments, contracted --------------------------
  test <- FALSE
  if(!test) {
    PEFInv <- investments
    ICPE <- contracted
  } 
  if(test) {
    PEFInv <- read.csv2(fname_in_PEFInv,
                            stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    ICPE <- read.csv2(fname_in_ICPE,
                            stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }
  
  ## 2 - calculate customer totals --------------------------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(amount)) %>%
    ungroup

  ## 3 - calculate customer totals per sector ---------------------------------
  PEFInvSec <- PEFInv %>%
    group_by(Customer_ID, sector) %>%
    summarise(sec_total = sum(amount)) %>%
    mutate(cust_total = 0) %>%
    ungroup

  ## 4 - Add column cust totals -----------------------------------------------
  for (i in c(1:nrow(PEFInvSec))) {
    curcust <- as.character(PEFInvSec[i,1])
    PEFInvSec[i,4] <- PEFInvCust[PEFInvCust$Customer_ID == curcust, 2]
  }   

  ## 5 - Calculate percentage and add status ----------------------------------
  PEFInvSec <- mutate(PEFInvSec, perc = sec_total / cust_total)
  PEFInvSec <- mutate(PEFInvSec, status = "at_review")
  PEFInvPct <- select(PEFInvSec, -sec_total, -cust_total)

  ## 6 - Copy ICPE, select/rearrange IC-Sectors, add/set status "at_contract --
  PEFCtrPct <- ICPE %>%
    select(c(1,3:7,17,8,12,13,9,11,16,18:20,10,15,14)) %>%
    add_column(status = "at_contract")
  colnames(PEFCtrPct) <- gsub("_", " ", colnames(PEFCtrPct))
  colnames(PEFCtrPct)[11] <- "Heavy industry"

  ## 7 - Replace rows of PEFCtrPct with investments if these are available, --
  ## otherwise keep contracted percentages -----------------------------------
  y <- colnames(PEFCtrPct)[c(2:19)]
  for (i in c(1:nrow(PEFCtrPct))) {
    currow <- PEFCtrPct[i,]
    curcust <- as.character(currow[1])
    inv_present <- curcust %in% PEFInvPct$Customer_ID
    if(inv_present) {
      curinv <- PEFInvPct[PEFInvPct$Customer_ID == curcust,]
      currow$status <- "at_review"
      currow[,c(2:19)] <- 0
      for (j in seq_along(y)) {
        if(y[j] %in% curinv$sector) {
          currow[j+1] <- curinv$perc[curinv$sector == y[j]]
        }
      PEFCtrPct[i,] <- currow
      } 
    }
  }
  DF_Out <- PEFCtrPct
  colnames(DF_Out) <- gsub(" ", "_", colnames(DF_Out))
  
  ## 8 - Generate output (investment-percentages per customer/country --------
  Out <- DF_Out
  return(Out)
}