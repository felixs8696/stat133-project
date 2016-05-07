library(DataComputing)
prod_rev <- read.csv("./appl_prod.csv")
names(prod_rev)[names(prod_rev)=="Other.products"] <- "Other"
prod_rev <- prod_rev %>%
  mutate(Category=ifelse(as.numeric(substr(Category,4,5)>50),
                         paste(19,substr(Category,4,5),"-",substr(Category,1,2),sep=""),
                         paste(20,substr(Category,4,5),"-",substr(Category,1,2),sep="")))
prod_rev <- prod_rev %>%
  mutate(Category=gsub("Q1$","01-01",Category)) %>%
  mutate(Category=gsub("Q2$","04-01",Category)) %>%
  mutate(Category=gsub("Q3$","07-01",Category)) %>%
  mutate(Category=gsub("Q4$","10-01",Category))
names(prod_rev)[names(prod_rev)=="Category"] <- "Date"
prod_rev[is.na(prod_rev)] <- 0
ggplot(prod_rev,aes(x=lubridate::ymd(Date))) +
  geom_bar(aes(weight=100,fill='Music')) + 
  geom_bar(aes(weight=100-Music,fill='Software')) + 
  geom_bar(aes(weight=100-Music-Software, fill='Peripherals')) + 
  geom_bar(aes(weight=100-Music-Software-Peripherals,fill='Other')) +
  geom_bar(aes(weight=iPhone+iPad+iPod+Mac+Services,fill='Services')) + 
  geom_bar(aes(weight=iPhone+iPad+iPod+Mac,fill='Mac')) + 
  geom_bar(aes(weight=iPhone+iPad+iPod,fill='iPod')) + 
  geom_bar(aes(weight=iPhone+iPad,fill='iPad')) + 
  geom_bar(aes(weight=iPhone,fill='iPhone')) +
  labs(title="Apple % of Revenue by Product", x="Date", y="% of Revenue", fill='Product') +
  scale_fill_brewer(palette = "Set1")