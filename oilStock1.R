setwd('C:/Users/sprot/Documents/GitHub/quantFinance')

library(tidyverse)
library(Quandl)
library(lubridate)

opecRaw = Quandl("OPEC/ORB", api_key="G8F8fspfaYVHJzPLQP5e")
wtiRaw = Quandl("EIA/PET_RWTC_D", api_key="G8F8fspfaYVHJzPLQP5e")
energySector = read.csv('sp_energy_sector.csv')
sp500 = read.csv('spx1995_2018.csv')
stockPrices = read.csv('prices.csv')
fundamentals = read.csv('fundamentals.csv')

#?ymd

opec1 = opecRaw %>%
  mutate(Date = ymd(Date),
         opecPrice = Value)

wti1 = wtiRaw %>%
  mutate(Date = ymd(Date),
         wtiPrice = Value)

energy1 = energySector %>%
  mutate(Date = mdy(ï..Effective.date),
         energyClosing = S.P.500.Energy..Sector.)


fundCVX = fundamentals %>%
  filter(Ticker.Symbol == 'CVX') %>%
  mutate(Date = ymd(Period.Ending))

sp5001 = sp500 %>%
  mutate(Date = mdy(Date),
         sp500Closing = Close)



priceCVX = read.csv('cvx.us.txt')
priceCVX = priceCVX %>%
  mutate(Date = ymd(Date))

join1 = merge(x = opec1, y = wti1, by = "Date", all.x = TRUE)
join2 = merge(x = join1, y = energy1, by = "Date", all.y = TRUE)
join3 = merge(x = join2, y = sp5001, by = "Date", all.x = TRUE)
join4 = merge(x = join3, y = fundCVX, by = "Date", all.x = TRUE)
join5 = merge(x = join4, y = priceCVX, by = "Date", all.x = TRUE)
