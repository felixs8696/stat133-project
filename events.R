library(DataComputing)
library(rvest)
library(lubridate)
library(XML)
applestocks <- read.csv("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&d=3&e=29&f=2016&g=d&a=11&b=12&c=1980&ignore=.csv") %>%
  mutate(appleclose=Close, Date=lubridate::ymd(Date), applevolume=Volume)
timeline <- htmlParse("http://www.timetoast.com/timelines/apple-product-release-dates")
root <- xmlRoot(timeline)
dates <- root %>% xpathSApply("//time",xmlValue)
date <- c()
for(i in 1:length(dates)){
  date <- c(date, toString(as.Date(dates[i], "%B %dst, %Y")))
}
desc <- root %>% xpathSApply("//td/b",xmlValue)
timeframe <- data.frame(date, desc) %>%
  mutate(date=lubridate::ymd(date))
ggplot(applestocks, aes(x=Date,y=appleclose)) + 
  geom_line() +
  geom_vline(data = timeframe, aes(xintercept = as.numeric(date), col=desc)) +
  theme(legend.text=element_text(size = 10),
        plot.title = element_text(size = 15)) +
  labs(title="Apple Stock Prices and Product Release Dates",x="Date", y="Stock Close", col='Event')