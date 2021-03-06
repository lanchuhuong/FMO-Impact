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
  PEFInv <- investments
  ICPE <- contracted

  ## 2 - calculate customer totals --------------------------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(outstanding)) %>%
    ungroup

  ## 3 - calculate customer totals per sector ---------------------------------
  PEFInvSec <- PEFInv %>%
    group_by(Customer_ID, sector) %>%
    summarise(sec_total = sum(outstanding)) %>%
    mutate(cust_total = 0) %>%
    ungroup

  ## 4 - Add column cust totals -----------------------------------------------
  for (i in c(1:nrow(PEFInvSec))) {
    PEFInvSec[i,4] <- PEFInvCust$cust_total[
      PEFInvCust$Customer_ID == PEFInvSec$Customer_ID[i]]
  }   

  ## 5 - Calculate percentage and add status ----------------------------------
  PEFInvSec <- mutate(PEFInvSec, perc = sec_total / cust_total)
  PEFInvSec <- mutate(PEFInvSec, status = "at_review")
  PEFInvPct <- select(PEFInvSec, -sec_total, -cust_total)
  rm(PEFInvSec)

  ## 6 - Copy ICPE, select/rearrange IC-Sectors, add/set status "at_contract --
  PEFCtrPct <- ICPE %>%
    select(c(1:6,16,7,11,12,8,10,15,17:19,9,14,13)) %>%
    add_column(status = "at_contract")
  # colnames(PEFCtrPct)[11] <- "Heavy industry"

  ## 7 - Replace rows of PEFCtrPct with investments if these are available, --
  ##     otherwise keep contracted asset percentages -------------------------
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

  ## 8 - Generate output (investment-percentages per customer/country --------
  Out <- PEFCtrPct
  return(Out)
}