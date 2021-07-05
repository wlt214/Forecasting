###########FORECASTING : Final Project (Energy Demand by State)#####################
#R shortcuts "%>%" is ctrl + shift + m /// "<-" is ALT + -
####################################
#The Global Energy Forecasting Competition in 2017 involved data on hourly zonal loads of ISO New England from March 2003 to April 2017. 
# The data have already been packaged into tibble format by Cameron Roach in the gefcom2017data Github repository. 
# So it is relatively easy to convert this to a tsibble.
# http://www.drhongtao.com/gefcom/2017
#drybulb is the temperature
#dewpnt is the dew point or humidity
####################################
install.packages("tibbletime")
library(tibbletime)
library(dplyr)
library(fpp3)

# Clear all variables in work space
rm(list=ls())  
typeof(gefcom)
############

# make tsibble() for gefcom data, code from source
gefcom2017 <- gefcom %>%
  ungroup() %>%
  as_tsibble(key=zone, index = ts)

gefcom2017
#frequency is 24; hourly data
frequency(gefcom2017)
# limit to 2007 - 2017
energy <- gefcom2017 %>% 
  filter(year(ts) >= 2007)
frequency(vic_elec)
vic_elec
################################################
#Visualizations of state data, state_energy
state_energy <- energy %>%
  filter(zone %in% c("CT", "MASS", "ME", "NH", "RI", "VT"))
autoplot(state_energy, demand)
#tsibble has tools to understand and tackle missing values
has_gaps(state_energy, .full = TRUE)
state_hourly_gaps <- state_energy %>%
  count_gaps(.full = TRUE)
state_hourly_gaps

#visualize gaps
ggplot(state_hourly_gaps, aes(x = zone, colour = zone)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")

#fill_gaps
state_energy_fill <- state_energy %>% 
  fill_gaps(.full = TRUE)
state_energy_fill
#rename state_energy_fill back to state_energy
state_energy <- state_energy_fill
#autoplot of demand in energy_full
autoplot(state_energy, demand)

#now able to plot daily energy(!)
state_energy %>% gg_season(demand, period = "day") +
  theme(legend.position = "right") +
  labs(y="Demand (MW)", title = "Gef Com - Electricity Demand by State Zones")
# resulting gg_season plot shows daily energy demand by zone/state with total
# notice VT energy is less than just western Mass.
################################################
#temperature over the years
state_energy %>% 
  filter(year(ts) == 2016) %>% 
  autoplot(drybulb) +
  labs(
    y = "Temperature",
    title = "Hourly temperatures by State, 2016"
  )

#plotting demand against temperature(dry bulb)
state_energy %>% 
  filter(year(ts) == 2016) %>% 
  ggplot(aes(x = drybulb, y = demand, color=zone)) +
  geom_point() +
  labs(x = "Temperature", y = "Electricity demand")
#state_energy hourly
state_energy %>% gg_season(demand, period = "day") +
  theme(legend.position = "right") +
  labs(y="Demand (MW)", title = "Gef Com - Electricity Demand by State")

#state_energy by day of the week
state_energy %>% gg_season(demand, period = "week") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Weekly Energy Demand by State")

#state_energy by year
state_energy %>% gg_season(demand, period = "year") +
  theme(legend.position = "left") +
  labs(y = "Demand (MW)", title = "Annual Energy Demand by State")

state_energy %>% 
  gg_subseries(demand) +
  labs(y = "Demand (MW)", title = "Energy Demand by State")

#Simple Forecast methods
#MEAN method
state_energy %>% model(MEAN(demand)) %>% 
  group_by(zone)
#NAIVE method
state_energy %>% model(NAIVE(demand)) %>% 
  group_by(zone)

#Seasonal NAIVE method
state_energy %>% model(SNAIVE(demand ~ lag("year"))) %>% 
  group_by(zone)

#Drift method
state_energy %>% model(RW(demand ~ drift())) %>% 
  group_by(zone)
aus_production

#Set training data from 2007 to 2016
train <- state_energy %>% 
  filter_index("2007-01-01 00:00:00" ~ "2014-12-31 23:00:00")
# Fit the models
state_fit <- train %>% 
  model(
    Mean = MEAN(demand),
    `Naïve` = NAIVE(demand),
    `Seasonal naïve` = SNAIVE(demand),
    `RW` = RW(demand)
  )
#GENERATE FORECASTS FOR 1 year, by hourly
state_fc <- state_fit %>% forecast(h = 8760)

#Plot forecasts against actual values
state_fc %>% 
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(state_energy, "2007-01-01 00:00:00"),
    colour = "black"
  ) + 
  labs(
    y = "Demand (MW)",
    title = "Forecasts for annual energy demand"
  ) + 
  guides(colour = guide_legend(title = "Forecast"))

#State - Addressing Missing values
st_miss <- state_energy %>% 
  anti_join

#Seasonal ARIMA
vermont <- state_energy %>% 
  filter(zone == "VT") %>% 
  select(ts, demand)
autoplot(vermont, demand, colour = "green") +
  labs(title = "Vermont Energy Demand",
       y = "Demand (MW)")
vermont %>%
  gg_tsdisplay(difference(demand, 365),
               plot_type='partial', lag=36) +
  labs(title = "Seasonally differenced", y = "")
#^still non-stationary so take further difference
# Also tried with 24
vermont %>% 
  gg_tsdisplay(difference(demand, 365) %>% difference(),
               plot_type = 'partial', lag = 36) + 
  labs(title = "Double Differenced", y = "")

fit_VT <- vermont %>% 
  model(
    arima012011 = ARIMA(demand ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(demand ~ pdq(2,1,1) + PDQ(0,1,1)),
    auto = ARIMA(demand, stepwise = FALSE, approx = FALSE)
    )
fit_VT %>% pivot_longer(everything(), names_to = "Model name",
                        values_to = "Orders")
glance(fit_VT) %>% arrange(AICc,.by_group = TRUE) %>% select(.model:BIC)

fit_VT %>% select(arima210011) %>% gg_tsresiduals(lag=36)
