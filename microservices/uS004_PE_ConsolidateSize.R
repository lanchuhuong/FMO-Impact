## ----------------------------------------------------------------------- 
## -- uS004_PE_ConsolidateSize -------------------------------------------- 
## ----------------------------------------------------------------------- 
## Generates investment percentage for SME/Corp for PEF-customers
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
## -- Input : Investments                       (5 columns)             -- 
## --       : Contracted                        (3 columns)             -- 
## -- Output: PE_InvPctPerSize                  (4 columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
uS004_PE_ConsolidateSize <- function(investments, contracted) {
  require(tidyverse)
  ## 1 - Read in parameters: investments, contracted --------------------------
  PEFInv <- investments
  ICPE <- contracted

  ## 2 - calculate customer totals --------------------------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(outstanding)) %>%
    ungroup

  ## 3 - calculate customer/sme totals ----------------------------------------
  PEFInvSme <- PEFInv %>%
    group_by(Customer_ID, sme) %>%
    filter(sme == "Yes") %>%
    summarise(sme_total = sum(outstanding)) %>%
    mutate(cust_total = 0) %>%
    ungroup

  ## 4 - Add column cust totals -----------------------------------------------
  for (i in c(1:nrow(PEFInvSme))) {
    curcust <- as.character(PEFInvSme[i,1])
    PEFInvSme[i,4] <- PEFInvCust$cust_total[PEFInvCust$Customer_ID == curcust]
  }   

  ## 5 - Calculate percentage and add status ----------------------------------
  PEFInvSme <- mutate(PEFInvSme, msme = sme_total / cust_total)
  PEFInvSme <- mutate(PEFInvSme, status = "at_review")
  PEFInvPct <- select(PEFInvSme, -sme_total, -cust_total, -sme)
  rm(PEFInvSme)

  ## 6 - Transform contracted and add status ----------------------------------
  ## please note the 'strange' test on Agriculture (ref. excel-tab IDFP - E40)
  ## remove ifelse if not needed..!
  PEFCtrPct <- ICPE
  PEFCtrPct$MFI[which(is.na(PEFCtrPct$MFI))] <- 0
  PEFCtrPct$SME[which(is.na(PEFCtrPct$SME))] <- 0
  PEFCtrPct <- PEFCtrPct %>%
    mutate(MFI = as.numeric(MFI)) %>%
    mutate(msme = (ifelse(is.na(Agriculture),NA,(SME + MFI)))) %>%
    select(1,23) %>%
    add_column(status = "at_contract")

  ## 7 - Remove contracted percentages if actual investments are present ------
  c <- PEFCtrPct$Customer_ID %in% PEFInvPct$Customer_ID
  PEFCtrPct <- filter(PEFCtrPct, !c)
  
  ## 8 - combine actual and contracted investement percentages ----------------
  DF_Out <- union(PEFInvPct, PEFCtrPct)
  DF_Out <- mutate(DF_Out, corporate = (1 - msme))
  DF_Out <- DF_Out[,c(1,2,4,3)]

  ## 9 - Generate output (investment-percentages per size ---------------------
  Out <- arrange(DF_Out, Customer_ID)
  ## Out <- DF_Out
  return(Out)
}