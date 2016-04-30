library(DataComputing)
library(lubridate)
applestocks <- read.csv("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&d=3&e=29&f=2016&g=d&a=11&b=12&c=1980&ignore=.csv")
microsoftstocks <- read.csv("http://real-chart.finance.yahoo.com/table.csv?s=MSFT&d=3&e=30&f=2016&g=d&a=2&b=13&c=1986&ignore=.csv")
applestocks<- applestocks %>% mutate(appleclose=Close, Date=lubridate::ymd(Date), applevolume=Volume)
microsoftstocks<- microsoftstocks %>% mutate(microsoftclose=Close, Date=lubridate::ymd(Date), microsoftvolume=Volume)
joined_stocks <- microsoftstocks %>% select(Date, microsoftclose, microsoftvolume) %>%
  inner_join(applestocks %>% select(Date, appleclose,applevolume), by=c("Date"="Date"))
ggplot(joined_stocks, aes(x=Date))+geom_point(aes(y=appleclose), color="red")
