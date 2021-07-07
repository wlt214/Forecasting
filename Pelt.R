library(tibbletime)
library(forecast)
library(dplyr)
library(fpp3)
library(tsibbledata)

tsibbledata::ata::pelt

pelt <- pelt

autoplot(pelt, Hare) 

#######Decomposition on Hare
dcmp <- pelt %>% 
  model(stl = STL(Hare))
components(dcmp)

components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(Hare) +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Hares",
    title = "Hares in the World"
  )

components(dcmp) %>% autoplot()
decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

########################
#STL decomposition
dcmp %>% 
  model(
    STL(Hare ~ trend(window = 1) +
              season(window = "periodic"),
    robust = TRUE)) %>% 
  components() %>% 
  autoplot()

########################
AP %>%
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

###Mean
mean_pelt <- pelt %>% model(MEAN(Hare))
autoplot(pelt)
###NAIVE
pelt %>% model(NAIVE(Hare))



bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

bricks %>% model(MEAN(Bricks))

autoplot(bricks)

############Forecasting with decomposition 5.7
dcmp <- pelt %>%
  model(STL(Lynx ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "Lynx",
       title = "Lynx Traded")

#decomposition_model() 5.7 
###stuck on this look up what window does
fit_dcmp <- pelt %>%
  model(stlf = decomposition_model(
    STL(lynx ~ trend(window = 70), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(pelt)+
  labs(y = "Lynx",
       title = "Lynx Traded")
###########STOPPED HERE

AP <- read.csv("/Users/willisthompson/Desktop/DAPT New Computer/DAPT 632 - Forecasting/Final Project/airpassengers2.csv", header = TRUE)

air <- AP
air %>% 
  mutate(Month = yearmonth(date)) %>% 
  as_tsibble(index = Month)
air %>%
  model(
    STL(date ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

class(air)
data$ts <- as.Date(air$date)
