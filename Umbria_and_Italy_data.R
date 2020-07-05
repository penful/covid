#### Extract covid data on the GITHUB repository ####
repository <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"

#### DATA ITALY #####
overall.dataset <- "dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
overall.filename<-paste(repository,overall.dataset,sep="")
#### all categories ####
Italy<-read.csv(overall.filename)
#### only the 5 selected categories ####
names(Italy)
library(dplyr)
dataItaly<- Italy %>%
 select(dimessi_guariti,isolamento_domiciliare,
 ricoverati_con_sintomi,
 terapia_intensiva,deceduti)

dataItaly<-dataItaly[1:128,]
#save(dataItaly, file = "dataItaly.Rdata")

#### DATA UMBRIA ####
regional.dataset <- "dati-regioni/dpc-covid19-ita-regioni.csv"
regional.filename<-paste(repository,regional.dataset,sep="")
regions<-read.csv(regional.filename)

myregion<-"Umbria"
myDateTimeStr1 <-regions$data[regions$denominazione_regione==myregion]
myPOSIXct1 <- as.POSIXct(myDateTimeStr1, format="%Y-%m-%dT %H:%M:%S")
days <- as.Date(myPOSIXct1)
step.ahead<-1
days.ahead <- seq(days[length(days)]+1,days[length(days)]+step.ahead,by=1)
require(xts)

select = "dimessi_guariti"
y <- as.numeric(unlist(subset(regions,
                              subset = (denominazione_regione == myregion),
                              select = select)))
count1<-xts(c(y,rep(NA,step.ahead)), order.by = c(days,days.ahead), frequency = 7)

select = "isolamento_domiciliare"
y <- as.numeric(unlist(subset(regions,
                              subset = (denominazione_regione == myregion),
                              select = select)))
count2<-xts(c(y,rep(NA,step.ahead)), order.by = c(days,days.ahead), frequency = 7)
#
select = "ricoverati_con_sintomi"
y <- as.numeric(unlist(subset(regions,
                              subset = (denominazione_regione == myregion),
                              select = select)))
count3<-xts(c(y,rep(NA,step.ahead)), order.by = c(days,days.ahead), frequency = 7)
#
select = "terapia_intensiva"
y <- as.numeric(unlist(subset(regions,
                              subset = (denominazione_regione == myregion),
                              select = select)))
count4<-xts(c(y,rep(NA,step.ahead)), 
             order.by = c(days,days.ahead), frequency = 7)
#
select = "deceduti"
y <- as.numeric(unlist(subset(regions,
                              subset = (denominazione_regione == myregion),
                              select = select)))
count5<-xts(c(y,rep(NA,step.ahead)), order.by = c(days,days.ahead), frequency = 7)
#
dataUmbria<-cbind(count1,count2,count3,count4,count5)
n<-dim(dataUmbria)[1]; n
dataUmbria<-data.frame(dataUmbria[-n,])
tail(dataUmbria)
names(dataUmbria)<-c("dimessi_guariti",
                 "isolamento_domiciliare",
                 "ricoverati_con_sintomi",
                 "terapia_intensiva",
                 "deceduti")
dataUmbria<-dataUmbria[1:128,]
#save(dataUmbria, file = "dataUmbria.Rdata")
