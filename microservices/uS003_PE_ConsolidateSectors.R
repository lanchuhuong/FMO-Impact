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

  ## 2 - calculate customer/country totals ------------------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(amount)) %>%
    ungroup

  ## 3 - calculate customer totals --------------------------------------------
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
  rm(PEFInvSec, PEFInvCust)

  ## 6 - Add status to contracted ---------------------------------------------
  PEFCtrPct <- ICPE %>%
    select(1,3:20) %>%
    gather(2:19, key = "sector", value = "perc") %>%
    add_column(status = "at_contract")

  ## 7 - Remove contracted percentages if actual investments are present ------
  c <- PEFCtrPct$Customer_ID %in% PEFInvPct$Customer_ID
  PEFCtrPct <- filter(PEFCtrPct, !c)
  PEFCtrPct <- PEFCtrPct[which(!is.na(PEFCtrPct$perc)),]

  ## 8 - combine actual and contracted investement percentages ----------------
  DF_Out <- union(PEFInvPct, PEFCtrPct)
  
  ## 9 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(DF_Out, Customer_ID, sector)
  ## Out <- DF_Out
  return(Out)
}