library(OECD)
library(dplyr)
library(tidyr)
library(zoo)

test<-get_dataset("QNA",
filter = list(c("AUT","DNK","FIN","IRL","LUX","PRT","SWE"),
              ("B1_GE"),
              ("VPVOBARSA"),
              ("Q")),
              #start_time = 2008,
              #end_time = 2010
              )

growth_rates<-test %>%
  select(LOCATION,obsTime,obsValue) %>%
  mutate(quarter = (substring(obsTime,7,7)),
         year = as.numeric(substring(obsTime,1,4))) %>%
  spread(LOCATION,obsValue) %>%
  mutate(small_eu = AUT + DNK + FIN + IRL + PRT + SWE) %>%
  gather("Country","Value",4:11) %>%
  arrange(Country, year, quarter) %>%
  group_by(Country) %>%
  mutate(growth = ( Value / lag(Value, n=1) - 1) * 100 ) %>%
  mutate(four_q_growth = ((Value +
    lag(Value,n=1) +
    lag(Value,n=2) +
    lag(Value,n=3)
    ) / (
    lag(Value,n=4) +
    lag(Value,n=5) +
    lag(Value,n=6) +
    lag(Value,n=7)
    ) - 1 ) * 100  ) %>%
  filter(year >= 2007) %>% 
  na.omit()

growth_rates %>%
  write.csv("latest_small_eu_growth.csv",
  row.names=FALSE)
