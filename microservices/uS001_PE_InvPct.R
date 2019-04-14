## ----------------------------------------------------------------------- 
## -- uS001_PE_InvPct ---------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Generates investment percentage per country per PEF-customers
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This function calculates investment percentage per country for each 
## PEF-customer. First it tries to find actual investments (at review)
## as present in "Investments". If no actual investments are made
## than we try to use the indicated countries at the time of contracting
## as present in "Contracted".
##
## INPUT PARAMETERS / OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : Investments                       (5 columns)             -- 
## --       : Contracted                        (3 columns)             -- 
## -- LUT   : Map_CntrReg                       (2 columns)             -- 
## -- Output: PE_InvPctPerCountry               (4 columns)             -- 
## ----------------------------------------------------------------------- 
##
## LOGIC
## check for rows in PEFRegPct that are also in InvRegPct
## throw these rows out of PEFRegPct because we have actual investments
## in InvRegPct, then combine both sets and order them on Cust_id and country
uS001_PE_InvPct <- function(investments, contracted, countrymap) {
  require(tidyverse)
  ## 1 - Read in parameters: PEFInv, PEFREg -----------------------------------
  PEFInv <- investments
  PEFReg <- contracted
  lutC <- countrymap

  ## 1 - calculate customer totals of investments -----------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(outstanding)) %>%
    ungroup

  ## 2 - In PEFInv: convert customer-countries into GHG-countries -------------
  PEFInv <- mutate(PEFInv, GHG_country = NA)
  for (i in seq_along(PEFInv$Customer_ID)) {
    PEFInv$GHG_country[i] <- lutC$Model_Region[lutC$Country == 
                                                 PEFInv$country[i]]
  }
  PEFInv <- select(PEFInv, -country)
  PEFInv <- select(PEFInv, c(1, 5, 2:4))
  
  ## 3 - calculate customer/GHG-country totals of investments -----------------
  PEFInvCtr <- PEFInv %>%
  group_by(Customer_ID, GHG_country) %>%
  summarise(ctr_total = sum(outstanding)) %>%
  ungroup

  ## 4 - Add column cust totals to investments per customer/country -----------
  PEFInvCtr <- mutate(PEFInvCtr, cust_total = 0)
  for (i in c(1:nrow(PEFInvCtr))) {
    curcust <- PEFInvCtr$Customer_ID[i]
    PEFInvCtr[i,4] <- PEFInvCust$cust_total[PEFInvCust$Customer_ID == curcust]
  }   

  ## 5 - Calculate inv. percentage per cust/country and add status ------------
  PEFInvCtr <- mutate(PEFInvCtr, perc = ctr_total / cust_total)
  PEFInvCtr <- mutate(PEFInvCtr, status = "at_review")
  PEFInvPct <- select(PEFInvCtr, -ctr_total, -cust_total)
  rm(PEFInvCtr, PEFInvCust)

  ## Over to regional asset percentages at time of contract
  ## 6 - Remove empty asset-percentages for Cust/Region-combi's and add status
  # PEFReg <- PEFReg[!is.na(PEFReg$AssetPerc),]
  PEFReg$AssetPerc <- as.numeric(PEFReg$AssetPerc)
  PEFRegPct <- PEFReg %>%
    add_column(status = "at_contract")
  colnames(PEFRegPct)[2] <- "GHG_country"
  colnames(PEFRegPct)[3] <- "perc"

  ## 7 - Remove contracted percentages if actual investments are present ------
  c <- PEFRegPct$Customer_ID %in% PEFInvPct$Customer_ID
  PEFRegPct <- filter(PEFRegPct, !c)

  ## 8 - combine actual and contracted investement percentages ----------------
  DF_Out <- union(PEFInvPct, PEFRegPct)

  ## 9 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(DF_Out, Customer_ID, GHG_country)
  PEFInvPct <- Out
  return(Out)
}