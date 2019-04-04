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
## throw these rows out of PEFRegPct because we have better information
## then combine both sets and order them on Cust_id and country
uS001_PE_InvPct <- function(investments, contracted) {
  require(tidyverse)
  ## 1 - Read in parameters: PEFInv, PEFREg -----------------------------------
  test <- FALSE
  if(!test) {
    PEFInv <- investments
    PEFReg <- contracted
  } 
  if(test) {
    PEFInv <- read.csv2(fname_in_PEFInv,
                        stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    PEFReg <- read.csv2(fname_in_PEFReg,
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }

  ## 2 - calculate customer totals of investments -----------------------------
  PEFInvCust <- PEFInv %>%
    group_by(Customer_ID) %>%
    summarise(cust_total = sum(amount)) %>%
    ungroup

  ## 3 - calculate customer/country totals of investments ---------------------
  PEFInvCtr <- PEFInv %>%
    group_by(Customer_ID, country) %>%
    summarise(ctr_total = sum(amount)) %>%
    mutate(cust_total = 0) %>%
    ungroup

  ## 4 - Add column cust totals to investments per customer/country -----------
  for (i in c(1:nrow(PEFInvCtr))) {
    curcust <- as.character(PEFInvCtr[i,1])
    PEFInvCtr[i,4] <- PEFInvCust[PEFInvCust$Customer_ID == curcust,2]
  }   

  ## 5 - Calculate inv. percentage per cust/country and add status ------------
  PEFInvCtr <- mutate(PEFInvCtr, perc = ctr_total / cust_total)
  PEFInvCtr <- mutate(PEFInvCtr, status = "at_review")
  PEFInvPct <- select(PEFInvCtr, -ctr_total, -cust_total)
  rm(PEFInvCtr, PEFInvCust)

  ## over to contracted regional percentages where no investments exist yet
  ## note: countries in this part differ from countries in investments-part
  ## PEFInv, PEFInvPct: use src_countries
  ## PEFReg, PEFRegPct: use GHG_countries
  
  ## 6 - Add status to contracted regional percentages ------------------------
  PEFRegPct <- PEFReg %>%
    add_column(status = "at_contract")
  colnames(PEFRegPct)[2] <- "country"
  colnames(PEFRegPct)[3] <- "perc"

  ## 7 - Remove contracted percentages if actual investments are present ------
  c <- PEFRegPct$Customer_ID %in% PEFInvPct$Customer_ID
  PEFRegPct <- filter(PEFRegPct, !c)

  ## 8 - combine actual and contracted investement percentages ----------------
  DF_Out <- union(PEFInvPct, PEFRegPct)
  
  ## 9 - map percentages from country to GHG-country --------------------------
  ## Note that lutC maps from src_country to GHG_country, but also supports
  ## GHG_country to GHG_country, that's why we could combine Inv & Reg.
  lutC <- read.csv2("datalake/luts/Map_CntrReg_2019-03-31.csv",
                   stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8")
  DF_Out <- mutate(DF_Out, GHG_country = NA)
  for (i in c(1:nrow(DF_Out))) {
    country <- as.character(DF_Out[i,2])
    DF_Out[i,5] <- lutC[lutC$Country == country, 2]
  }   
  DF_Out <- DF_Out %>%
    group_by(Customer_ID, GHG_country, status) %>%
    summarise(inv_percentage = sum(perc)) %>%
    ungroup

  ## 10 - Generate output (investment-percentages per customer/country --------
  Out <- arrange(DF_Out, Customer_ID, GHG_country)
  return(Out)
}