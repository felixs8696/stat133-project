install.packages("png")
library(DataComputing)
library(png)
library(grid)
library(reshape2)
library(XML)
library(RCurl)
library(lubridate)
library(ggplot2)
URL<-"http://csimarket.com/stocks/balance.php?code=AAPL&hist=1"
txt<- getURLContent(URL)
doc<- htmlParse(txt)
numbers<- doc%>%xpathSApply('//td[@class="debeligrub2"]/strong/span', xmlValue)
subpath1<-'//td[@class="svjetlirub"]/span'
numbers2<- doc%>%xpathSApply(subpath1, xmlValue)

Total_Assets<-numbers[11:15]
Total_Current_Assets<- numbers[6:10]
EquipmentPropertyPlantNet<- numbers2[41:45]
Total_Liabilities<- numbers[26:30]
Quarter<- doc%>%xpathSApply('//td[@class="s9 zagqs sve_jedan_red"]', xmlValue)
totalassetspath<- '//td[@class="debeligrub2"]/strong/span'

readvalue= 
  function(doc, path, number)
  {
    info <- doc %>% xpathSApply(path, xmlValue)
    info[number]
  }

addtotalassets<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  addtotalassets<- c(addtotalassets, readvalue(doc, totalassetspath, 15))
}

Total_Assets <-c(Total_Assets, addtotalassets)
addtotalcurrentassets<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  addtotalcurrentassets<- c(addtotalcurrentassets, readvalue(doc, totalassetspath, 10))
}
Total_Current_Assets<-c(Total_Current_Assets, addtotalcurrentassets)

addEquipment<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  addEquipment<- c(addEquipment, readvalue(doc, subpath1, 45))
}
EquipmentPropertyPlantNet<-c(EquipmentPropertyPlantNet, addEquipment)

addliabilities<-c()
for (i in 2:44) {
  URL<-paste0("http://csimarket.com/stocks/balance.php?code=AAPL&hist=",i)
  txt<- getURLContent(URL)
  doc<- htmlParse(txt)
  addliabilities<- c(addliabilities, readvalue(doc, totalassetspath, 30))
}
Total_Liabilities<-c(Total_Liabilities, addliabilities)
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

day<-c()
month<-c()
year<-c()
for (i in 1:length(strsplit(appleassets$date, "-"))) {
  day<-c(day, strsplit(appleassets$date, "-")[[i]][2])
  month<-c(month, strsplit(appleassets$date, "-")[[i]][1])
  year<-c(year, strsplit(appleassets$date, "-")[[i]][3])
}
lan<-c("Sep", "June", "March", "Dec")
num<-c("09", "06", "03", "12")
translation<- data.frame(lan, num)
translation<-left_join(as.data.frame(month), translation, by=c("month"="lan"))
month<-translation[,2]
appleassets<- data.frame(Date=lubridate::ymd(paste0(year,"-", month, "-", day)), appleassets) %>% select(Date, Total_Assets) %>% mutate(Total_Assets=gsub(pattern = ".000$", "", Total_Assets))
TotalAssets<-as.numeric(gsub(pattern = ",", "", Total_Assets))/1000
TotalCurrentAssets<-as.numeric(gsub(pattern = ",", "", Total_Current_Assets))/1000
TotalLiabilities<-as.numeric(gsub(pattern = ",", "", Total_Liabilities))/1000
EquipmentPropertyPlantNet<-as.numeric(gsub(pattern = ",", "", EquipmentPropertyPlantNet))/1000
appleassets<- appleassets %>% data.frame(TotalAssets, EquipmentPropertyPlantNet, TotalLiabilities)
APPLE<- melt(appleassets%>%select(Date, TotalAssets, EquipmentPropertyPlantNet, TotalLiabilities), id="Date", value.name = "AmountinBillions")
download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Apple_logo_black.svg/768px-Apple_logo_black.svg.png", destfile = "applelogo.png", mode="wb")
img<-readPNG("applelogo.png")
g<- rasterGrob(img, interpolate=TRUE)
ggplot(APPLE, aes(x=Date, y=AmountinBillions))+geom_smooth(aes(color=variable, fill=variable))+labs(y="Amount (in Billions)", title="2004-2016")+theme(axis.title.x=element_text(size=20), 
                                                                                                                                                       axis.title.y=element_text(size=20), 
                                                                                                                                                       plot.title=element_text(size=30),
                                                                                                                                                       panel.grid.major=element_blank(),
                                                                                                                                                       panel.background=element_rect(fill="white"),
                                                                                                                                                       panel.grid.minor=element_blank())+geom_point(shape=1, aes(color=variable))+
  annotation_custom(g, ymin=250, ymax=300)
