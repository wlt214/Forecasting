###############FORECASTING: FINAL PROJECT (Massachusetts Daily Energy Demand)
#settled on Mass. daily data, as a state, not region not other states
############################
#drybulb is the temperature
#dewpnt is the dew point or humidity
####################################
install.packages("tibbletime")
library(tibbletime)
library(dplyr)
library(fpp3)
library(forecast)

gefcom
# make tsibble() for gefcom data, code from source
gefcom2017 <- gefcom %>%
  ungroup() %>%
  as_tsibble(key=zone, index = ts)
#frequency is 24; hourly data
frequency(gefcom2017)
# limit to 2007 - 2017
energy <- gefcom2017 %>% 
  filter(year(ts) >= 2007)
#filter to Massachusetts regions
mass <- energy %>%
  filter(zone %in% c("MASS"))
#fill_gaps
mass <- mass %>% 
  fill_gaps(.full = TRUE)

##***Change to Daily Data####
mass_daily <- mass %>%
  #filter(year(ts) == (c(2014,2015,2016))) %>%
  index_by(Date = date(ts)) %>%
  summarise(
    demand = sum(demand) / 1e3,
    temperature = max(drybulb),
    holiday = any(holiday)
  ) 
#%>%
#  mutate(Week_Day_Type = case_when(
#    holiday ~ "Holiday",
#    wday(Date) %in% 2:6 ~ "Weekday",
#    TRUE ~ "Weekend"
#  ))
#Seasonal Plots (2.4)
#Mass energy hourly
mass %>% gg_season(demand, period = "day") +
  theme(legend.position = "none") +
  labs(x = "time of day", y="Demand (MW)", title = "Mass. Energy Demand by hour")
#mass by day of the week
mass %>% gg_season(demand, period = "week") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Weekly Energy Demand in Mass.")

#mass by year
mass %>% gg_season(demand, period = "year") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Annual Energy Demand in Mass.")

mass %>% 
  gg_subseries(demand) +
  labs(y = "Demand (MW)", title = "Massachusetts Energy Demand by State")
#Daily Seasonal Plots (2.4)
#Mass energy hourly
mass_daily %>% gg_season(demand, period = "day") +
  theme(legend.position = "none") +
  labs(x = "time of day", y="Demand (MW)", title = "Mass. Energy Demand by hour")
#mass by day of the week
mass_daily %>% gg_season(demand, period = "week") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Weekly Energy Demand in Mass.")

#mass by year
mass_daily %>% gg_season(demand, period = "year") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Annual Energy Demand in Mass.")

mass %>% 
  gg_subseries(demand) +
  labs(y = "Demand (MW)", title = "Massachusetts Energy Demand by State")

###############
library(seasonal)
massNA <- mass
massNA[c(2,15)]<- NA

m <- seas(massNA, na.action = na.x13)
head(m$data)
#STL
mass %>% 
  model(
    STL(demand ~ trend(window(mass, 2007)) +
                      season(window(mass)),
    robust = TRUE)) %>% 
  components() %>% 
  autoplot()

#SEATS method (3.5)(is not possible)
seats_dcmp <- mass %>% 
  model(seats = X_13ARIMA_SEATS(demand ~ seats())) %>% 
  components()
autoplot(seats_dcmp) + 
  labs(title = "Decomposition of Mass. Energy demand using SEATS")

##Complex Seasonality (12.1)(did not work due to missing values)
complexSTL_mass <- mass %>% mutate(t = row_number()) %>% 
  update_tsibble(index = ts, regular = TRUE)

complexSTL_mass %>% 
  model(
    STL(sqrt(demand) ~ season(period = 169) +
                       season(period = 5*169),
        robust = TRUE)
  ) %>% 
  components() %>% 
  autoplot() + labs(x = "_____")

#12.1 Complex Seasonality (***WORKED***)
#could redo with 3 year time span like the book did
mass %>% 
  pivot_longer(demand:drybulb, names_to = "Series") %>% 
  ggplot(aes(x = ts, y = value)) +
  geom_line() + 
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "", title = "Electricity Demand and corresponding temperatures")

#Adding in Work Day filter
# work_mass is subset and adds columns to denote workdays vs weekend
work_mass <- mass %>% 
  mutate(
    DOW = wday(ts, label = TRUE),
    Working_Day = !holiday & !(DOW %in% c("Sat","Sun")),
    Cooling = pmax(drybulb, 65)
  )
work_mass %>% 
  ggplot(aes(x=drybulb, y=demand, col=Working_Day)) +
  geom_point(alpha = 0.6) +
  labs(x="Temperature", y="Demand (MWh)")
#REGRESSSION MODEL 
#review PDQ and pqd being set to (0,0,0)
fourier_work <- work_mass %>% 
  model(TSLM(demand ~ trend() + fourier(K = 2)))
report(fourier_work)

fit <- work_mass %>% 
  model(
    ARIMA(demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
            drybulb + Cooling + Working_Day +
            fourier(period = "day", K = 10) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )

##forecasting based on recently recorded values, using 2 previous days
work_newdata <- new_data(work_mass, 2*24) %>% 
  mutate(
    Temperature = tail(work_mass$drybulb, 2 * 24),
    Date = lubridate::as_date(ts),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2017-05-01") &
                    !(DOW %in% c("Sat","Sun")),
    Cooling = pmax(drybulb, 65)
  )
#fc  of fit with forecast function
fc <- fit %>% 
  forecast(new_data = work_newdata)

#autoplot of complex seasonality fit, adding a tail of 14 days
fc %>% 
  autoplot(work_mass %>% tail(10 * 24)) + 
  labs(title = "Massachusetts Energy Demand forecasted",
       y = "Demand (MWh)", x = "Time in Hours")
