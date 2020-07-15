library(readr)
covid19 <- read_csv("~/Documents/biostat/covid-19 project/covid19.txt")

coviddata<-subset(covid19,state==c("Oregon","Washington" ))
mydate <- factor(coviddata$date)
mydate<- as.Date(mydate, format = "%Y-%m-%d")
coviddata$date<- mydate- as.Date("2020-01-01","%Y-%m-%d")
# data_15 <- read_csv("~/Documents/biostat/covid-19 project/cc-est2018-alldata-15.csv")
# data_41 <- read_csv("~/Documents/biostat/covid-19 project/cc-est2018-alldata-41.csv")
# data_53 <- read_csv("~/Documents/biostat/covid-19 project/cc-est2018-alldata-53.csv")
# demo0<-rbind(data_15,data_41,data_53)


demo0<-read.csv("~/Documents/biostat/covid-19 project/cc-est2018-alldata-41.csv",header=T)
used.col <- c('STNAME','CTYNAME','AGEGRP','TOT_POP','TOT_MALE','TOT_FEMALE', 'WA_MALE','WA_FEMALE','BA_MALE','BA_FEMALE','AA_MALE','AA_FEMALE','H_MALE','H_FEMALE')

demo<- demo0[demo0$YEAR==11,] 
demo<- demo[, used.col]
total <- demo[demo$AGEGRP==0,]; 
Pmale<- total$TOT_MALE/total$TOT_POP
Pwhite<- (total$WA_MALE+total$WA_FEMALE)/total$TOT_POP 
Pblack<- (total$BA_MALE+total$BA_FEMALE)/total$TOT_POP 
Pasian<- (total$AA_MALE+total$AA_FEMALE)/total$TOT_POP 
Phispanic<- (total$H_MALE+total$H_FEMALE)/total$TOT_POP
age<-matrix(demo[, 4], ncol=19,byrow=T)
Page<- as.data.frame(age[,-1]/age[,1])
colnames(Page)= c(paste0("Page", 1:18))
demo.final2<-cbind(total[, c(1:2,4)], Pmale, Pwhite, Pblack, Pasian, Phispanic, Page);
write.csv(demo.final2,'Oregon.csv',row.names = F)

demo0<-read.csv("~/Documents/biostat/covid-19 project/cc-est2018-alldata-53.csv",header=T)
used.col <- c('STNAME','CTYNAME','AGEGRP','TOT_POP','TOT_MALE','TOT_FEMALE', 'WA_MALE','WA_FEMALE','BA_MALE','BA_FEMALE','AA_MALE','AA_FEMALE','H_MALE','H_FEMALE')

demo<- demo0[demo0$YEAR==11,] 
demo<- demo[, used.col]
total <- demo[demo$AGEGRP==0,]; 
Pmale<- total$TOT_MALE/total$TOT_POP
Pwhite<- (total$WA_MALE+total$WA_FEMALE)/total$TOT_POP 
Pblack<- (total$BA_MALE+total$BA_FEMALE)/total$TOT_POP 
Pasian<- (total$AA_MALE+total$AA_FEMALE)/total$TOT_POP 
Phispanic<- (total$H_MALE+total$H_FEMALE)/total$TOT_POP
age<-matrix(demo[, 4], ncol=19,byrow=T)
Page<- as.data.frame(age[,-1]/age[,1])
colnames(Page)= c(paste0("Page", 1:18))
demo.final1<-cbind(total[, c(1:2,4)], Pmale, Pwhite, Pblack, Pasian, Phispanic, Page);
write.csv(demo.final1,'Washington.csv',row.names = F)

demo<-rbind(demo.final1,demo.final2)
write.csv(demo,'~/Documents/biostat/covid-19 project/DemoData.csv',row.names = F)

# weather
X35_tmax_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/35-tmax-202003-1.csv", 
                              skip = 3)
or.weather.max<-X35_tmax_202003_1[,1:3]
colnames(or.weather.max)<-c("state", "county", "MaxTemp")

X35_tmin_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/35-tmin-202003-1.csv", 
                              skip = 3)
or.weather.min<-X35_tmin_202003_1[,1:3]
colnames(or.weather.min)<-c("state", "county", "MinTemp")
or.weather<-merge(or.weather.max,or.weather.min,by=c("state","county"))

X35_pcp_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/35-pcp-202003-1.csv", 
                              skip = 3)
or.weather.pcp<-X35_pcp_202003_1[,1:3]
colnames(or.weather.pcp)<-c("state", "county", "precipitation")
or.weather<-merge(or.weather,or.weather.pcp,by=c("state","county"))
or.weather$state <- rep("Oregon",length(or.weather$county))

X45_tmax_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/45-tmax-202003-1.csv", 
                              skip = 3)
wa.weather.max<-X45_tmax_202003_1[,1:3]
colnames(wa.weather.max)<-c("state", "county", "MaxTemp")

X45_tmin_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/45-tmin-202003-1.csv", 
                              skip = 3)
wa.weather.min<-X45_tmin_202003_1[,1:3]
colnames(wa.weather.min)<-c("state", "county", "MinTemp")
wa.weather<-merge(wa.weather.max,wa.weather.min,by=c("state","county"))

X45_pcp_202003_1 <- read_csv("~/Documents/biostat/covid-19 project/45-pcp-202003-1.csv", 
                             skip = 3)
wa.weather.pcp<-X45_pcp_202003_1[,1:3]
colnames(wa.weather.pcp)<-c("state", "county", "precipitation")
wa.weather<-merge(wa.weather,wa.weather.pcp,by=c("state","county"))

wa.weather$state <- rep("Washington",length(wa.weather$county))
weather<-rbind(wa.weather,or.weather)
write.csv(weather,'~/Documents/biostat/covid-19 project/weather.csv',row.names = F)

#combine
#step1
library(readxl)
poverty <- read_excel("~/Documents/biostat/covid-19 project/PovertyEstimates.xls")
colnames(poverty)<-c("FIPStxt","state","county","PpovertyALL_2018","Ppoverty017_2018")
poverty.OR<-subset(poverty,state==c("OR" ))
poverty.OR$state <- rep("Oregon",length(poverty.OR $county))
poverty.WA<-subset(poverty,state==c("WA" ))
poverty.WA$state <- rep("Washington",length(poverty.WA$county))
poverty<-rbind(poverty.WA,poverty.OR)
#install.packages("roperators")
library(roperators)
poverty$county %-=% ' County'
unique(poverty$county)
unique(coviddata$county)
combineddata<-merge(coviddata,poverty,by=c("state","county"))
unique(combineddata$county)

#step2
unemployment <- read_excel("~/Documents/biostat/covid-19 project/Unemployment.xls")
colnames(unemployment)<-c("FIPS","state","county","Unemployment_rate_2018", "Med_HH_Income_2018","Med_HH_Income_vs_Total_2018")
unemployment.OR<-subset(unemployment,state==c("OR" ))
unemployment.OR$state <- rep("Oregon",length(unemployment.OR $county))
unemployment.WA<-subset(unemployment,state==c("WA" ))
unemployment.WA$state <- rep("Washington",length(unemployment.WA$county))
unemployment<-rbind(unemployment.WA,unemployment.OR)
unemployment$county %-=% ' County, WA'
unemployment$county %-=% ' County, OR'
combineddata<-merge(combineddata,unemployment,by=c("state","county"))
unique(combineddata$county)

#step3
education <- read_excel("~/Documents/biostat/covid-19 project/Education.xls")
colnames(education)<-c("FIPS Code","state","county","Pnohighschool1418","Phighschool1418","Psomecollege1418","Pcollege1418")
education.OR<-subset(education,state==c("OR" ))
education.OR$state <- rep("Oregon",length(education.OR $county))
education.WA<-subset(education,state==c("WA" ))
education.WA$state <- rep("Washington",length(education.WA$county))
education<-rbind(education.WA,education.OR)
education$county %-=% ' County'

combineddata<-merge(combineddata,education,by=c("state","county"))
unique(combineddata$county)

#step4
population<- read_excel("~/Documents/biostat/covid-19 project/PopulationEstimates.xls")
colnames(population)<-c("FIPS","state","county","R_birth_2018","R_death_2018","R_INTERNATIONAL_MIG_2018","R_DOMESTIC_MIG_2018" )
population.OR<-subset(population,state==c("OR" ))
population.OR$state <- rep("Oregon",length(population.OR$county))
population.WA<-subset(population,state==c("WA" ))
population.WA$state <- rep("Washington",length(population.WA$county))
population<-rbind(population.WA,population.OR)
population$county %-=% ' County'

combineddata<-merge(combineddata,population,by=c("state","county"))
unique(combineddata$county)

#step5
colnames(demo)<-c("state","county","TOT_POP","Pmale","Pwhite","Pblack","Pasian","Phispanic","Page1","Page2","Page3", "Page4","Page5","Page6",    
    "Page7","Page8","Page9","Page10","Page11","Page12","Page13","Page14","Page15","Page16","Page17","Page18"  )
demo.OR<-subset(demo,state==c("Oregon" ))
demo.WA<-subset(demo,state==c("Washington" ))
demo<-rbind(demo.WA,demo.OR)
demo$county %-=% ' County'
combineddata<-merge(combineddata,demo,by=c("state","county"))
unique(combineddata$county)

#step6
weather$county %-=% ' County'
combineddata<-merge(combineddata,weather,by=c("state","county"))
unique(combineddata$county)

#step7
colnames(combineddata)
combineddata$Med_HH_Income_2018 <- as.numeric(gsub('\\$|,', '',
combineddata$Med_HH_Income_2018))

##analysis
#install.packages("lme4")
library(lme4)
stdz<-function(x) (x-mean(x))/sd(x) # standardize the predictors
case.model<- glmer(cases ~ state
                   +stdz(Page1)+stdz(Page2)+stdz(Page3)+stdz(Page4)
                   +stdz(Page5)+stdz(Page6)+stdz(Page7)+stdz(Page8)+stdz(Page9)
                   +stdz(Page10)+stdz(Page11)+stdz(Page12)+stdz(Page13)
                   +stdz(Page14)+stdz(Page15)+stdz(Page16)+stdz(Page17)
                   +stdz(Pmale)+stdz(Pwhite)+stdz(Pblack)
                   +stdz(Pasian)+stdz(Phispanic)+stdz(MaxTemp)+stdz(MinTemp)+stdz(precipitation)
                   +stdz(PpovertyALL_2018)+stdz(Ppoverty017_2018)+stdz(Unemployment_rate_2018)
                   +stdz(Med_HH_Income_2018)+stdz(Med_HH_Income_vs_Total_2018)
                   +stdz(Pnohighschool1418)+stdz(Phighschool1418)
                   +stdz(Psomecollege1418)+stdz(Pcollege1418)
                   +(1+stdz(date) |county), nAGQ = 0L,
                   offset = TOT_POP/1000000, data =combineddata,family = poisson)
summary(case.model)  
  
death.model<- glmer(deaths ~ state
                   +stdz(Page1)+stdz(Page2)+stdz(Page3)+stdz(Page4)
                   +stdz(Page5)+stdz(Page6)+stdz(Page7)+stdz(Page8)+stdz(Page9)
                   +stdz(Page10)+stdz(Page11)+stdz(Page12)+stdz(Page13)
                   +stdz(Page14)+stdz(Page15)+stdz(Page16)+stdz(Page17)
                   +stdz(Pmale)+stdz(Pwhite)+stdz(Pblack)
                   +stdz(Pasian)+stdz(Phispanic)+stdz(MaxTemp)+stdz(MinTemp)+stdz(precipitation)
                   +stdz(PpovertyALL_2018)+stdz(Ppoverty017_2018)+stdz(Unemployment_rate_2018)
                   +stdz(Med_HH_Income_2018)+stdz(Med_HH_Income_vs_Total_2018)
                   +stdz(Pnohighschool1418)+stdz(Phighschool1418)
                   +stdz(Psomecollege1418)+stdz(Pcollege1418)
                   +(1+stdz(date) |county), nAGQ = 0L,
                   offset =(cases+0.5)/1000000, data =combineddata,family = poisson)
summary(death.model)  


  
  
  
  
  





