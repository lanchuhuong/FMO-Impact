## Script for plotting GHG-emissions
## GVersteeg, April 10th, 2019
##
## IN/OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : KPI_aGHG.csv                    (247 / 7 )                -- 
## ----------------------------------------------------------------------- 
##
## ----------------------------------------------------------------------- 
## --- ORGANIZE PROCESSING ENVIRONMENT ----------------------------------- 
## ----------------------------------------------------------------------- 
## Loading libraries
## Note: require is used inside functions, as it outputs a warning and 
## continues if the package is not found, whereas library will throw an error
library(tidyverse)

## Setup locations
insite <- ""
rawdir <- "datalake/raw_data/"
lutdir <- "datalake/luts/"
clndir <- "datalake/clean_data/"
valdir <- "datalake/valid_data/"
inddir <- "datalake/indicators/"
dptdir <- "datalake/data_points/"
laydir <- "datalake/infoproducts/GIS_layers/"
qlkdir <- "datalake/infoproducts/Qlik_cubes/"
sopdir <- "datalake/infoproducts/sopact_sheets/"

## Setup parameters
version <- ""
indate = "2019-03-31"
lutdate = ""
outdate = "2019-04-10"

## Setup filenames
fname_in_AE <- paste0(inddir,"KPI_aGHG", "_", indate,".csv")
fname_out_custPF <- paste0(clndir,"CustPortfolio", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- AE/AEyr ------ Creating and Plotting GHG Emission data ------------
## ----------------------------------------------------------------------- 
## 1 - read file from xlsx-tab, remove last line (totals)
AE <- read.csv2(fname_in_AE,
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
AE <- AE[!is.na(AE$Ems),]
hist(AE$Ems)

## 2 - generate emissions previous years, considering (1.5C pathway): 
## current emission (2013) = 33.000 MtCO2e/year
## targeted emission (2025) = 23.000 MtCO2e/year
## targeted yearly reduction = 10.000/13 = 770 MtCO2e
## which is aprrox. 3% CO2e reduction each year
perc <- 3
peru <- perc/100
sdev <- abs(peru)
AEyr <- AE %>%
  select(Ems) %>%
  rename(Yr_2018 = Ems) %>%
  mutate(Yr_2017 = (Yr_2018 * (1 + rnorm(1, mean=peru, sd=sdev)))) %>%
  mutate(Yr_2016 = (Yr_2017 * (1 + rnorm(1, mean=peru, sd=sdev)))) %>%
  mutate(Yr_2015 = (Yr_2016 * (1 + rnorm(1, mean=peru, sd=sdev))))
check_4 <- round(100-(sum(AEyr$Yr_2015)/sum(AEyr$Yr_2018)*100), 2)

## 3 - gathering the resulting emissions & remove outliers
AEg <- AEyr
colnames(AEg) <- gsub("Yr_", "", colnames(AEg))
AEg <- gather(AEg, `2018`, `2017`,`2016`,`2015`,
              key = "Year", value = "ems")
AEg$Year <- as.numeric(AEg$Year)
AEg <- AEg[AEg$ems < 2500, ]
  
## 3 - plotting the emissions
g <- ggplot(data=AEg, aes(x=Year, y=ems)) +
  scale_x_continuous(breaks = seq(2015,2025,1),
                     lim = c(2015,2025)) +
  geom_point(size=0.2 , color="Blue") +
  stat_smooth(method = "lm", formula = y ~ x, color="Darkred",
              fullrange = TRUE)
g

# mdl <- lm(AEg, formula = ems ~ as.vector(Year))
# summary(mdl)
# newyr <- data.frame(Year = as.numeric(c(2019, 2020, 2021, 2022,
#                                         2023, 2024, 2025)))
# z <- predict(mdl, as.vector(newyr), interval = "confidence")


## 4 - write source data
## write.csv2(GenInd, fname_out_GenInd, row.names = FALSE,
##            fileEncoding = "UTF-8")
