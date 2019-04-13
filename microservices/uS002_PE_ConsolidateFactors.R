## ----------------------------------------------------------------------- 
## -- uS002_PE_ConsolidateFactors --------------------------------------------------- 
## ----------------------------------------------------------------------- 
## Generates investment percentage per country per PEF-customers
## GVersteeg, March 28th, 2019
##
## DESCRIPTION
## This function consolidates investment percentages by given factors.
## These factors can be PK-factors (Capital Intensity, CpIF) or Economic
## Emission Factors (Scope 1, EcEF1 or Scope 2, EcEF2).
##
## INPUT PARAMETERS / OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : Investment-percentages             (4 columns)            -- 
## --       : Factors (PK, EF-scope1, EF-scope1) (23 cntrs / 20 sctrs)  -- 
## -- LUT   : Map_CntrReg                        (2 columns)            -- 
## -- Output: Consolidated factors (PK,EF1,EF2)  (4 columns)            -- 
## ----------------------------------------------------------------------- 
##
## LOGIC

uS002_PE_ConsolidateFactors <- function(invperc, factors, countrymap) {
  require(tidyverse)
  require(stringr)
  ## 1 - Read in parameters ----------------------------------------------
  IP <- invperc
  CF <- factors
  lutC <- countrymap

  ## 2 - complete the PEFInvPct dataframe with NA's ----------------------
  cntr <- str_sort(unique(lutC$Model_Region))
  cntr <- cntr[c(1:17,19,18,20:23)]
  cust <- unique(IP$Customer_ID)
  IPcpl <- data.frame(Customer_ID = character(0),
                   GHG_country = character(0),
                   inv_percentage = numeric(0),
                   status = character(0),
                   stringsAsFactors = FALSE)
  r <- 1
  for (i in c(1:length(cust))) {
    for (j in c(1:length(cntr))) {
      IPcpl[r,1] <- cust[i]
      IPcpl[r,2] <- cntr[j]
      IPcpl[r,3] <- 0
      IPcpl[r,4] <- NA
      r <- r + 1
    }
  }  
  IPcpl <- unite(IPcpl, Customer_ID, GHG_country,
               col = "combi", sep = "-", remove = FALSE)
  IP <- unite(IP, Customer_ID, GHG_country, 
                      col = "combi", sep = "-", remove = FALSE)
  for (i in c(1:nrow(IPcpl))) {
    current <- IPcpl$combi[i]
    found <- current %in% IP$combi
    IPcpl[i,4] <- ifelse(found, IP[IP$combi == current, 4], 0)
    IPcpl[i,5] <- ifelse(found, IP[IP$combi == current, 5], NA)
  }
  IPcpl <- select(IPcpl, -combi, -status)
  IPcpl <- spread(IPcpl, key = GHG_country, value = inv_percentage)
  IPcpl <- IPcpl[,c(1:18,20,19,21:24)]
  
  ## 3 - Check matrix multiplication (rows1 = cols2) --------------------------
  ReadyToGo <- identical(colnames(IPcpl)[-1], CF$GHG_Country)
  ## Only Not ReadyTo Go if exactly the same (count and entries)

  ## 4 - Setup matrix (countries/sectors) -------------------------------------
  Mtx1 <- as.matrix(IPcpl[,-1])
  Mtx2 <- as.matrix(CF[,-1])
  
  ## 5 - Multiply matrices and convert into data frame (cust/sectors) --------
  Mtx3 <- Mtx1 %*% Mtx2
  Out <- as.data.frame(Mtx3)
  Out <- mutate(Out, Customer_ID = cust)
  x <- ncol(Out)
  y <- x - 1
  Seq <- c(x, 1:y)
  Out <- Out[, Seq]

  ## 6 - Output data frame (cust/sectors) ------------------------------------
  PEFConsPKF <- Out
  return(Out)
}