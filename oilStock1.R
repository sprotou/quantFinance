setwd('C:/Users/sprot/Documents/GitHub/quantFinance/quantFinance')
#load packages and datasets------------------
library(tidyverse)
library(Quandl)
library(lubridate)
library(forecast)
library(TTR)
library(keras)
library(h2o)        # Awesome ML Library
library(timetk)    

opecRaw = Quandl("OPEC/ORB", api_key="G8F8fspfaYVHJzPLQP5e")
wtiRaw = Quandl("EIA/PET_RWTC_D", api_key="G8F8fspfaYVHJzPLQP5e")
energySector = read.csv('sp_energy_sector.csv')
sp500 = read.csv('spx1995_2018.csv')
stockPrices = read.csv('prices.csv')
fundamentals = read.csv('fundamentals.csv')
sonicData = read.csv('sonic_data.csv')


#data manipulation and cleaning---------------
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

sonic1 = sonicData %>%
  mutate(Date = mdy(Date))

#current data range: April 2008 to May 2018

priceCVX = read.csv('cvx.us.txt')
priceCVX = priceCVX %>%
  mutate(Date = ymd(Date))

join1 = merge(x = opec1, y = wti1, by = "Date", all.x = TRUE)
join2 = merge(x = join1, y = energy1, by = "Date", all.y = TRUE)
join3 = merge(x = join2, y = sp5001, by = "Date", all.x = TRUE)
join4 = merge(x = join3, y = fundCVX, by = "Date", all.x = TRUE)
join5 = merge(x = join4, y = priceCVX, by = "Date", all.x = TRUE) #join5 is my "working" dataset for now
join6 = merge(x = join5, y = sonic1, by = "Date", all.x = TRUE) #join6: sonic's dataset

?ma




#data summaries---------------
glimpse(join5)
summary(join5)



#EDA--------------------------

cor(x = join5$opecPrice, y = join5$wtiPrice, use='complete.obs')
#even though the correlation is extremely high at 0.9716, it is not safe to say we can only use one of the variables.
#Think of the business context: if OPEC increase supply and price drops as a result, WTI immediately reacts to it. 
#So there may be a causality here that can be useful for our analysis.
cor(x = join5$opecPrice, y = join5$energyClosing, use='complete.obs') # 0.54
cor(x = join5$opecPrice, y = join5$Close.y, use='complete.obs')# 0.09 !!!

ggplot(join5) + geom_point(mapping = aes(x=opecPrice, y = energyClosing)) #energy sector closing price
ggplot(join5) + geom_point(mapping = aes(x=opecPrice, y = Close.y)) #Close.y is closing price for CVX
ggplot(join5) + geom_point(mapping = aes(x=wtiPrice, y = energyClosing))
ggplot(join5) + geom_point(mapping = aes(x=wtiPrice, y = Close.y))
 
ggplot(join5) + geom_point(mapping = aes(x=energyClosing, y = Close.y)) #CVX vs Energy index
#notice the "islands" formed. Could it be different years have had different ratio of x/y?

#time series----------------

#decomposition (moving average because its probably not seasonal)
cvxTs = ts(join5$Close.y)
movAvgCvx7 = rollapply(cvxTs, 7, mean, na.rm = TRUE)
movAvgCvx30 = rollapply(cvxTs, 30, mean, na.rm = TRUE)
movAvgCvx100 = rollapply(cvxTs, 100, mean, na.rm = TRUE)

#ggplot(movAvgCvx7) + geom_line(aes)
plot.ts(movAvgCvx7)
plot.ts(movAvgCvx30)
plot.ts(movAvgCvx100)



#moving averages

join5 = join5 %>%
  mutate(movAvg7 = ma(opecPrice, 7),
         movAvg30 = ma(opecPrice, 30),
         movAvg100 = ma(opecPrice, 100))


ggplot(join5) + geom_line(mapping = aes(x=Date, y = Close.y, colour = "CVX")) +
  geom_line(mapping = aes(x=Date, y = energyClosing/4, colour = "energyClosing/4")) + #/4 just so that we dont stretch the scales, for visibility
  geom_line(mapping = aes(x=Date, y = opecPrice, colour = "opecPrice")) +
  geom_line(mapping = aes(x=Date, y = wtiPrice, colour = "wtiPrice"))
  
  
#write.csv(join6, file = 'spyros_dataset_export.csv')  
# ggplot(join5) +  geom_line(mapping = aes(x=Date, y = opecPrice, colour = "opecPrice")) +
#   geom_line(mapping = aes(x=Date, y = movAvg7, colour = "movAvg7")) +
#   geom_line(mapping = aes(x=Date, y = movAvg30, colour = "movAvg30")) +
#   geom_line(mapping = aes(x=Date, y = movAvg100, colour = "movAvg100"))

#lag hypotheses: how long later does oil price affect the stocks prices?--------------

#production by country---------
countryProd = read.csv('by_country.csv')

countryProd = countryProd %>%
  mutate(country = ï..LOCATION)

countryProdGrp = countryProd %>%
  filter(country %in% c('WLD','USA','SAU','RUS')) #to be fixed

library(tidyr)
?dcast
?spread
#spread(countryProdGrp, country, )

join7 = join6 %>%
  tk_augment_timeseries_signature()
glimpse(join7)

h2oClean <- join7 %>%
  select_if(~ !is.Date(.)) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

h2oClean %>% glimpse()

train_tbl <- h2oClean %>% filter(year < 2017)
valid_tbl <- h2oClean %>% filter(year == 2017)
test_tbl  <- h2oClean %>% filter(year == 2018)

h2o.init()

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "energySector"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = energySector, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance")
