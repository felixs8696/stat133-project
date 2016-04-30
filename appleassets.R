library(DataComputing)
library(XML)
library(RCurl)
URL<-"http://csimarket.com/stocks/balance.php?code=AAPL&hist=1"
txt<- getURLContent(URL)
doc<- htmlParse(txt)
numbers<- doc%>%xpathSApply('//td[@class="debeligrub2"]/strong/span', xmlValue)
Total_Assets<-numbers[11:15]
Quarter<- doc%>%xpathSApply('//td[@class="s9 zagqs sve_jedan_red"]', xmlValue)
Quarter

readvalue= 
  function(doc)
  {
    info <- doc %>% xpathSApply('//td[@class="debeligrub2"]/strong/span', xmlValue)
    info[15]
  }

add<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  add<- c(add, readvalue(doc))
}

Total_Assets <-c(Total_Assets, add)
readdate= 
  function(doc)
  {
    info <- doc %>% xpathSApply('//td[@class="s9 zagqs sve_jedan_red"]', xmlValue)
    info[5]
  }

date<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  date<- c(date, readdate(doc))
}
date<-c(Quarter,date)

appleassets<-data.frame(date, Total_Assets)
appleassets<- appleassets %>%
  mutate(date=gsub("[(,),.]|I|V|Quarter|", "", date))
appleassets<- appleassets %>% mutate(date=gsub(" ", "-", date))
appleassets<-appleassets %>% mutate(date=gsub('.{1}$', "", date))
appleassets[5, 1] = "Sep-27-2014"

date<-c()
month<-c()
year<-c()
for (i in 1:length(strsplit(appleassets$date, "-"))) {
  date<-c(date, strsplit(appleassets$date, "-")[[i]][2])
  month<-c(month, strsplit(appleassets$date, "-")[[i]][1])
  year<-c(year, strsplit(appleassets$date, "-")[[i]][3])
}
lan<-c("Sep", "June", "March", "Dec")
num<-c("09", "06", "03", "12")
translation<- data.frame(lan, num)
translation<-left_join(as.data.frame(month), translation, by=c("month"="lan"))
month<-translation[,2]
appleassets<- data.frame(Date=lubridate::ymd(paste0(year,"-", month, "-", date)), appleassets) %>% select(Date, Total_Assets) %>% mutate(Total_Assets=gsub(pattern = ".000$", "", Total_Assets))
TotalAssets<-gsub(pattern = ",", "", Total_Assets)
appleassets %>% mutate(Total_Assets=TotalAssets)
ggplot(appleassets, aes(x=Date, y=as.numeric(TotalAssets)))+geom_smooth()+labs(y="Total Assets", title="Apple's Total Assets")
