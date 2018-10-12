####Task 1ii####
library(readxl)
dfDealer_Satisfaction <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = " Dealer Satisfaction", skip = 2)
dfEnd_User_Satisfaction <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "End-User Satisfaction", skip = 2)
dfComplaints <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Complaints", skip = 2)
dfMower_Unit_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Mower Unit Sales", skip = 2)
dfTractor_Unit_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Tractor Unit Sales", skip = 2)

#Dealer Satisfaction####
dfDealer_Satisfaction$'Weighted Average'<-dfDealer_Satisfaction$`1`+2*dfDealer_Satisfaction$`2`+3*dfDealer_Satisfaction$`3`+4*dfDealer_Satisfaction$`4`+5*dfDealer_Satisfaction$`5`
dfDealer_Satisfaction$'Weighted Average'<-dfDealer_Satisfaction$'Weighted Average'/ as.numeric(dfDealer_Satisfaction$Sample)

ds.na<-cbind(dfDealer_Satisfaction$'Survey Scale:'[2:6], dfDealer_Satisfaction$`Weighted Average`[2:6])
plot(ds.na, type='o', col='red', xlab='Year', ylab='Weighted Average Dealer Satisfaction', main='Dealer Satisfaction Chart Across Years', ylim=c(3,5))

ds.sa<-cbind(dfDealer_Satisfaction$'Survey Scale:'[9:13], dfDealer_Satisfaction$`Weighted Average`[9:13])
lines(ds.sa,type='o', col='green')

ds.eu<-cbind(dfDealer_Satisfaction$'Survey Scale:'[16:20], dfDealer_Satisfaction$`Weighted Average`[16:20])
lines(ds.eu,type='o', col='blue')

ds.pr<-cbind(dfDealer_Satisfaction$'Survey Scale:'[23:27], dfDealer_Satisfaction$`Weighted Average`[23:27])
lines(ds.pr,type='o', col='black')

ds.ch<-cbind(dfDealer_Satisfaction$'Survey Scale:'[30:32], dfDealer_Satisfaction$`Weighted Average`[30:32])
lines(ds.ch,type='o', col='purple')

legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

#End-User Satisfaction####
dfEnd_User_Satisfaction$'Weighted Average'<-dfEnd_User_Satisfaction$`1`+2*dfEnd_User_Satisfaction$`2`+3*dfEnd_User_Satisfaction$`3`+4*dfEnd_User_Satisfaction$`4`+5*dfEnd_User_Satisfaction$`5`
dfEnd_User_Satisfaction$'Weighted Average'<-dfEnd_User_Satisfaction$'Weighted Average'/ as.numeric(dfEnd_User_Satisfaction$Size)

eus.na<-cbind(dfEnd_User_Satisfaction$`North America`[1:5], dfEnd_User_Satisfaction$`Weighted Average`[1:5])
plot(eus.na, type='o', col='red', xlab='Year', ylab='Weighted Average End-User Satisfaction', main='End-User Satisfaction Chart Across Years', ylim=c(3.7,4.3))

eus.sa<-cbind(dfEnd_User_Satisfaction$`North America`[8:12], dfEnd_User_Satisfaction$`Weighted Average`[8:12])
lines(eus.sa,type='o', col='green')

eus.eu<-cbind(dfEnd_User_Satisfaction$`North America`[15:19], dfEnd_User_Satisfaction$`Weighted Average`[15:19])
lines(eus.eu,type='o', col='blue')

eus.pr<-cbind(dfEnd_User_Satisfaction$`North America`[22:26], dfEnd_User_Satisfaction$`Weighted Average`[22:26])
lines(eus.pr,type='o', col='black')

eus.ch<-cbind(dfEnd_User_Satisfaction$`North America`[29:31], dfEnd_User_Satisfaction$`Weighted Average`[29:31])
lines(eus.ch,type='o', col='purple')

legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

#Complaints####
dfComplaints$Month<- as.character.Date(dfComplaints$Month)

c.byYear<- matrix(nrow = 5, ncol = 6)
c.byYear[,1]<-2010:2014
row.names(c.byYear)<-c(2010:2014)

for(i in 1:5){
  c.byYear[i,2]<-sum(dfComplaints$'NA'[dfComplaints$Month>c.byYear[i,1] & dfComplaints$Month<c.byYear[i,1]+1])
  c.byYear[i,3]<-sum(dfComplaints$'SA'[dfComplaints$Month>c.byYear[i,1] & dfComplaints$Month<c.byYear[i,1]+1])
  c.byYear[i,4]<-sum(dfComplaints$'Eur'[dfComplaints$Month>c.byYear[i,1] & dfComplaints$Month<c.byYear[i,1]+1])
  c.byYear[i,5]<-sum(dfComplaints$'Pac'[dfComplaints$Month>c.byYear[i,1] & dfComplaints$Month<c.byYear[i,1]+1])
  c.byYear[i,6]<-sum(dfComplaints$'China'[dfComplaints$Month>c.byYear[i,1] & dfComplaints$Month<c.byYear[i,1]+1])
}

#Plot to compare numbers across regions
plot(c.byYear[,2], type='o', col='red', xlab='Year', ylab='Total Complaints', main='Number of Complaints', ylim=c(45, 1750), xaxt="n")
axis(1, at = c(1:5), labels = 2010:2014)
lines(c.byYear[,3],type='o', col='green')
lines(c.byYear[,4],type='o', col='blue')
lines(c.byYear[,5],type='o', col='black')
lines(c.byYear[,6],type='o', col='purple')
legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

#Plot to compare trend in each region
plot(cbind(c.byYear[,1],c.byYear[,2]), type='o', col='red',  xlab='Year', ylab='Total Complaints', main='Number of Complaints \nin North America')
plot(cbind(c.byYear[,1],c.byYear[,3]),type='o', col='green',  xlab='Year', ylab='Total Complaints', main='Number of Complaints \nin South America')
plot(cbind(c.byYear[,1],c.byYear[,4]),type='o', col='blue',  xlab='Year', ylab='Total Complaints', main='Number of Complaints \nin Europe')
plot(cbind(c.byYear[,1],c.byYear[,5]),type='o', col='black',  xlab='Year', ylab='Total Complaints', main='Number of Complaints \nin Pacific Rim')
plot(cbind(c.byYear[,1],c.byYear[,6]),type='o', col='purple',  xlab='Year', ylab='Total Complaints', main='Number of Complaints \nin China')

#Mower Unit Sales####
dfMower_Unit_Sales$Month<- as.character.Date(dfMower_Unit_Sales$Month)

mus.byYear<- matrix(nrow = 5, ncol = 6)
mus.byYear[,1]<-2010:2014
row.names(mus.byYear)<-c(2010:2014)

for(i in 1:5){
  mus.byYear[i,2]<-sum(dfMower_Unit_Sales$'NA'[dfMower_Unit_Sales$Month>mus.byYear[i,1] & dfMower_Unit_Sales$Month<mus.byYear[i,1]+1])
  mus.byYear[i,3]<-sum(dfMower_Unit_Sales$'SA'[dfMower_Unit_Sales$Month>mus.byYear[i,1] & dfMower_Unit_Sales$Month<mus.byYear[i,1]+1])
  mus.byYear[i,4]<-sum(dfMower_Unit_Sales$'Europe'[dfMower_Unit_Sales$Month>mus.byYear[i,1] & dfMower_Unit_Sales$Month<mus.byYear[i,1]+1])
  mus.byYear[i,5]<-sum(dfMower_Unit_Sales$'Pacific'[dfMower_Unit_Sales$Month>mus.byYear[i,1] & dfMower_Unit_Sales$Month<mus.byYear[i,1]+1])
  mus.byYear[i,6]<-sum(dfMower_Unit_Sales$'China'[dfMower_Unit_Sales$Month>mus.byYear[i,1] & dfMower_Unit_Sales$Month<mus.byYear[i,1]+1])
}

#Plot to compare numbers across regions
plot(mus.byYear[,2], type='o', col='red', xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales', ylim=c(110, 92000), xaxt="n")
axis(1, at = c(1:5), labels = 2010:2014)
lines(mus.byYear[,3],type='o', col='green')
lines(mus.byYear[,4],type='o', col='blue')
lines(mus.byYear[,5],type='o', col='black')
lines(mus.byYear[,6],type='o', col='purple')
legend('right', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

#Plot to compare trend in each region
plot(cbind(mus.byYear[,1],mus.byYear[,2]), type='o', col='red',  xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales \nin North America')
plot(cbind(mus.byYear[,1],mus.byYear[,3]),type='o', col='green',  xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales \nin South America')
plot(cbind(mus.byYear[,1],mus.byYear[,4]),type='o', col='blue',  xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales \nin Europe')
plot(cbind(mus.byYear[,1],mus.byYear[,5]),type='o', col='black',  xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales \nin Pacific Rim')
plot(cbind(mus.byYear[,1],mus.byYear[,6]),type='o', col='purple',  xlab='Year', ylab='Total Mower Unit Sales', main='Number of Mower Unit Sales \nin China')

#Tractor Unit Sales####
dfTractor_Unit_Sales$Month<- as.character.Date(dfTractor_Unit_Sales$Month)

tus.byYear<- matrix(nrow = 5, ncol = 6)
tus.byYear[,1]<-2010:2014
row.names(tus.byYear)<-c(2010:2014)

for(i in 1:5){
  tus.byYear[i,2]<-sum(dfTractor_Unit_Sales$'NA'[dfTractor_Unit_Sales$Month>tus.byYear[i,1] & dfTractor_Unit_Sales$Month<tus.byYear[i,1]+1])
  tus.byYear[i,3]<-sum(dfTractor_Unit_Sales$'SA'[dfTractor_Unit_Sales$Month>tus.byYear[i,1] & dfTractor_Unit_Sales$Month<tus.byYear[i,1]+1])
  tus.byYear[i,4]<-sum(dfTractor_Unit_Sales$'Eur'[dfTractor_Unit_Sales$Month>tus.byYear[i,1] & dfTractor_Unit_Sales$Month<tus.byYear[i,1]+1])
  tus.byYear[i,5]<-sum(dfTractor_Unit_Sales$'Pac'[dfTractor_Unit_Sales$Month>tus.byYear[i,1] & dfTractor_Unit_Sales$Month<tus.byYear[i,1]+1])
  tus.byYear[i,6]<-sum(dfTractor_Unit_Sales$'China'[dfTractor_Unit_Sales$Month>tus.byYear[i,1] & dfTractor_Unit_Sales$Month<tus.byYear[i,1]+1])
}

#Plot to compare numbers across regions
plot(tus.byYear[,2], type='o', col='red', xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales', ylim=c(200, 25000), xaxt="n")
axis(1, at = c(1:5), labels = 2010:2014)
lines(tus.byYear[,3],type='o', col='green')
lines(tus.byYear[,4],type='o', col='blue')
lines(tus.byYear[,5],type='o', col='black')
lines(tus.byYear[,6],type='o', col='purple')
legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

#Plot to compare trend in each region
plot(cbind(tus.byYear[,1],tus.byYear[,2]), type='o', col='red',  xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales \nin North America')
plot(cbind(tus.byYear[,1],tus.byYear[,3]),type='o', col='green',  xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales \nin South America')
plot(cbind(tus.byYear[,1],tus.byYear[,4]),type='o', col='blue',  xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales \nin Europe')
plot(cbind(tus.byYear[,1],tus.byYear[,5]),type='o', col='black',  xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales \nin Pacific Rim')
plot(cbind(tus.byYear[,1],tus.byYear[,6]),type='o', col='purple',  xlab='Year', ylab='Total Tractor Unit Sales', main='Number of Tractor Unit Sales \nin China')



####Task 2####
dfIndustry_Mower_Total_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Industry Mower Total Sales", skip = 2)
dfIndustry_Tractor_Total_Sales <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Industry Tractor Total Sales", skip = 2)
#Rename column names to be same as that in dfMower_Unit_Sales
colnames(dfIndustry_Mower_Total_Sales) <- c('Month', 'NA', 'SA', 'Europe', 'Pacific', 'World')
dfMower_Market_Share <- dfMower_Unit_Sales
dfTractor_Market_Share <- dfTractor_Unit_Sales

#Creating table of prices for PLE products
PLE.prices<-matrix(nrow = 5, ncol = 2)
PLE.prices[,1]<-c(150,175,180,185,190)
PLE.prices[,2]<-c(3250,3400,3600,3700,3800)
rownames(PLE.prices)<-2010:2014
colnames(PLE.prices)<-c('MowerPrice', 'TractorPrice')

#Gross Revenue####
##Mower Sales
#Generating columns for gross revenues of each region for Mower Sales
dfMower_Market_Share$'Year'<-substr(dfMower_Market_Share$Month,1,4)
for(i in c('NA', 'SA', 'Europe', 'Pacific', 'China', 'World')){
  dfMower_Market_Share[paste(i, 'Revenue')]<-dfMower_Market_Share[i] * PLE.prices[dfMower_Market_Share$Year,'MowerPrice']
}

#Plot to compare numbers across regions
plot(dfMower_Market_Share$`NA Revenue`, type='o', col='red', xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales', ylim=c(100,2000000), xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
lines(dfMower_Market_Share$`SA Revenue`,type='o', col='green')
lines(dfMower_Market_Share$`Europe Revenue`,type='o', col='blue')
lines(dfMower_Market_Share$`Pacific Revenue`,type='o', col='black')
lines(dfMower_Market_Share$`China Revenue`,type='o', col='purple')
legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

plot(dfMower_Market_Share$`World Revenue`, type='o', col='red', xlab='Year', ylab='Gross Revenue', main='World total Gross Revenue of Mower Sales', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

#Plot to compare trend in each region
plot(dfMower_Market_Share$`NA Revenue`, type='o', col='red',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales \nin North America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`SA Revenue`, type='o', col='green',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales \nin South America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`Europe Revenue`, type='o', col='blue',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales \nin Europe', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`Pacific Revenue`, type='o', col='black',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales \nin Pacific Rim', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`China Revenue`, type='o', col='purple',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Mower Sales \nin China', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

##Tractor Sales
#Generating columns for gross revenues of each region for Tractor Sales
dfTractor_Market_Share$'Year'<-substr(dfTractor_Market_Share$Month,1,4)
for(i in c('NA', 'SA', 'Eur', 'Pac', 'China', 'World')){
  dfTractor_Market_Share[paste(i, 'Revenue')]<-dfTractor_Market_Share[i] * PLE.prices[dfTractor_Market_Share$Year,'TractorPrice']
}

#Plot to compare numbers across regions
plot(dfTractor_Market_Share$`NA Revenue`, type='o', col='red', xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales', ylim=c(36000,9500000), xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
lines(dfTractor_Market_Share$`SA Revenue`,type='o', col='green')
lines(dfTractor_Market_Share$`Eur Revenue`,type='o', col='blue')
lines(dfTractor_Market_Share$`Pac Revenue`,type='o', col='black')
lines(dfTractor_Market_Share$`China Revenue`,type='o', col='purple')
legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

plot(dfTractor_Market_Share$`World Revenue`, type='o', col='red', xlab='Year', ylab='Gross Revenue', main='World total Gross Revenue of Tractor Sales', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

#Plot to compare trend in each region
plot(dfTractor_Market_Share$`NA Revenue`, type='o', col='red',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales \nin North America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`SA Revenue`, type='o', col='green',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales \nin South America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`Eur Revenue`, type='o', col='blue',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales \nin Europe', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`Pac Revenue`, type='o', col='black',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales \nin Pacific Rim', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`China Revenue`, type='o', col='purple',  xlab='Year', ylab='Gross Revenue', main='Gross Revenue of Tractor Sales \nin China', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

#Market Share####
##Mower Sales
for(i in c('NA', 'SA', 'Europe', 'Pacific', 'World')){
  dfMower_Market_Share[paste(i, 'Market Share')]<-dfMower_Market_Share[i] / dfIndustry_Mower_Total_Sales[i]
}

#Plot to compare numbers across regions
plot(dfMower_Market_Share$`NA Market Share`, type='o', col='red', xlab='Year', ylab='Market Share', main='Market Share of Mower Sales', ylim=c(0.04,0.5), xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
lines(dfMower_Market_Share$`SA Market Share`,type='o', col='green')
lines(dfMower_Market_Share$`Europe Market Share`,type='o', col='blue')
lines(dfMower_Market_Share$`Pacific Market Share`,type='o', col='black')
legend('topleft', legend=c('North America', 'South America', 'Europe', 'Pacific Rim'), fill = c('red', 'green', 'blue', 'black'))

plot(dfMower_Market_Share$`World Market Share`, type='o', col='red', xlab='Year', ylab='Market Share', main='World total Market Share of Mower Sales', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

#Plot to compare trend in each region
plot(dfMower_Market_Share$`NA Market Share`, type='o', col='red',  xlab='Year', ylab='Market Share', main='Market Share of Mower Sales \nin North America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`SA Market Share`, type='o', col='green',  xlab='Year', ylab='Market Share', main='Market Share of Mower Sales \nin South America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`Europe Market Share`, type='o', col='blue',  xlab='Year', ylab='Market Share', main='Market Share of Mower Sales \nin Europe', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfMower_Market_Share$`Pacific Market Share`, type='o', col='black',  xlab='Year', ylab='Market Share', main='Market Share of Mower Sales \nin Pacific Rim', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

##Tractor Sales
for(i in c('NA', 'SA', 'Eur', 'Pac', 'China', 'World')){
  dfTractor_Market_Share[paste(i, 'Market Share')]<-dfTractor_Market_Share[i] / dfIndustry_Tractor_Total_Sales[i]
}

#Plot to compare numbers across regions
plot(dfTractor_Market_Share$`NA Market Share`, type='o', col='red', xlab='Year', ylab='Market Share', main='Market Share of Tractor Sales', ylim=c(0.03,0.31), xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
lines(dfTractor_Market_Share$`SA Market Share`,type='o', col='green')
lines(dfTractor_Market_Share$`Eur Market Share`,type='o', col='blue')
lines(dfTractor_Market_Share$`Pac Market Share`,type='o', col='black')
lines(dfTractor_Market_Share$`China Market Share`,type='o', col='purple')
legend('left', legend=c('North America', 'South America', 'Europe', 'Pacific Rim', 'China'), fill = c('red', 'green', 'blue', 'black', 'purple'))

plot(dfTractor_Market_Share$`World Market Share`, type='o', col='red', xlab='Year', ylab='Market Share', main='World total Market Share of Tractor Sales', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

#Plot to compare trend in each region
plot(dfTractor_Market_Share$`NA Market Share`, type='o', col='red',  xlab='Year', ylab='Market Share', main='Market Share of Tractor \nSales in North America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`SA Market Share`, type='o', col='green',  xlab='Year', ylab='Market Share', main='Market Share of Tractor \nSales in South America', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`Eur Market Share`, type='o', col='blue',  xlab='Year', ylab='Market Share', main='Market Share of Tractor \nSales in Europe', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`Pac Market Share`, type='o', col='black',  xlab='Year', ylab='Market Share', main='Market Share of Tractor \nSales in Pacific Rim', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
plot(dfTractor_Market_Share$`China Market Share`, type='o', col='purple',  xlab='Year', ylab='Market Share', main='Market Share of Tractor \nSales in China', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)

write.csv(dfMower_Market_Share, file='Q2_dfMower_Market_Share.csv')
write.csv(dfTractor_Market_Share, file='Q2_dfTractor_Market_Share.csv')


####Task 3i####
dfEnd_User_Satisfaction$Region<-NA
dfEnd_User_Satisfaction$Region[1:5]<-'North America'
for(i in c(8,15,22, 29)){
  for(j in 0:4){
    dfEnd_User_Satisfaction$'Region'[i+j]<-dfEnd_User_Satisfaction$`North America`[i-1]
    }
}
dfEnd_User_Satisfaction<-dfEnd_User_Satisfaction[!is.na(dfEnd_User_Satisfaction$Region),]
colnames(dfEnd_User_Satisfaction)[1] <- 'Year'

dfDealer_Satisfaction$Region<-NA
for(i in c(2,9,16,23,30)){
  for(j in 0:4){
    dfDealer_Satisfaction$'Region'[i+j]<-dfDealer_Satisfaction$'Survey Scale:'[i-1]
  }
}
dfDealer_Satisfaction<-dfDealer_Satisfaction[!is.na(dfDealer_Satisfaction$Region),]
colnames(dfDealer_Satisfaction)[1] <- 'Year'


####Task 3ii####
dfEnd_User_Satisfaction$'Standard Deviation'<-0
for(score in 0:5){
  dfEnd_User_Satisfaction$'Standard Deviation'<-dfEnd_User_Satisfaction$'Standard Deviation'+ dfEnd_User_Satisfaction[score+2]*((score-dfEnd_User_Satisfaction$`Weighted Average`)^2)
}
dfEnd_User_Satisfaction$'Standard Deviation'<-dfEnd_User_Satisfaction$'Standard Deviation'/ (dfEnd_User_Satisfaction$Size-1)
dfEnd_User_Satisfaction$'Standard Deviation'<-sqrt(dfEnd_User_Satisfaction$'Standard Deviation')

dfDealer_Satisfaction$'Standard Deviation'<-0
for(score in 0:5){
  dfDealer_Satisfaction$'Standard Deviation'<-dfDealer_Satisfaction$'Standard Deviation'+ dfDealer_Satisfaction[score+2]*((score-dfDealer_Satisfaction$`Weighted Average`)^2)
}
dfDealer_Satisfaction$'Standard Deviation'<-dfDealer_Satisfaction$'Standard Deviation'/ (as.numeric(dfDealer_Satisfaction$'Sample')-1)
dfDealer_Satisfaction$'Standard Deviation'<-sqrt(dfDealer_Satisfaction$'Standard Deviation')


####Task 3iii####
df2014Customer_Survey <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "2014 Customer Survey", skip = 2)
library(psych)
describe(df2014Customer_Survey)


####Task 3iv####
freq.Quality<-table(df2014Customer_Survey$Region, df2014Customer_Survey$Quality)
freq.Quality<-cbind(freq.Quality, 0)
colnames(freq.Quality)[6]<-'Top Box Proportion'
freq.Quality[,6]<-(freq.Quality[,4]+freq.Quality[,5])/(freq.Quality[,1]+freq.Quality[,2]+freq.Quality[,3]+freq.Quality[,4]+freq.Quality[,5])
freq.Quality
barplot(freq.Quality[,'Top Box Proportion'], xlab='Region', ylab = 'Top Box Proportion', main = 'Quality')

freq.EaseofUse<-table(df2014Customer_Survey$Region, df2014Customer_Survey$`Ease of Use`)
freq.EaseofUse<-cbind(freq.EaseofUse, 0)
colnames(freq.EaseofUse)[6]<-'Top Box Proportion'
freq.EaseofUse[,6]<-(freq.EaseofUse[,4]+freq.EaseofUse[,5])/(freq.EaseofUse[,1]+freq.EaseofUse[,2]+freq.EaseofUse[,3]+freq.EaseofUse[,4]+freq.EaseofUse[,5])
freq.EaseofUse
barplot(freq.EaseofUse[,'Top Box Proportion'], xlab='Region', ylab = 'Top Box Proportion', main = 'Ease of Use')

freq.Price<-table(df2014Customer_Survey$Region, df2014Customer_Survey$Price)
freq.Price<-cbind(freq.Price, 0)
colnames(freq.Price)[6]<-'Top Box Proportion'
freq.Price[,6]<-(freq.Price[,4]+freq.Price[,5])/(freq.Price[,1]+freq.Price[,2]+freq.Price[,3]+freq.Price[,4]+freq.Price[,5])
freq.Price
barplot(freq.Price[,'Top Box Proportion'], xlab='Region', ylab = 'Top Box Proportion', main = 'Price')

freq.Service<-table(df2014Customer_Survey$Region, df2014Customer_Survey$Service)
freq.Service<-cbind(freq.Service, 0)
colnames(freq.Service)[6]<-'Top Box Proportion'
freq.Service[,6]<-(freq.Service[,4]+freq.Service[,5])/(freq.Service[,1]+freq.Service[,2]+freq.Service[,3]+freq.Service[,4]+freq.Service[,5])
freq.Service
barplot(freq.Service[,'Top Box Proportion'], xlab='Region', ylab = 'Top Box Proportion', main = 'Service')


####Task 3v####
t.test(df2014Customer_Survey$Quality, df2014Customer_Survey$`Ease of Use`)
t.test(df2014Customer_Survey$Quality, df2014Customer_Survey$Price)
t.test(df2014Customer_Survey$Quality, df2014Customer_Survey$Service)
t.test(df2014Customer_Survey$`Ease of Use`, df2014Customer_Survey$Price)
t.test(df2014Customer_Survey$`Ease of Use`, df2014Customer_Survey$Service)
t.test(df2014Customer_Survey$Price, df2014Customer_Survey$Service)



####Task 4i####
dfMower_Test <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Mower Test", skip = 3)
Number.Passed<-0
for(i in as.character(1:30)){Number.Passed=Number.Passed + table(dfMower_Test[i])['Pass']}
1- Number.Passed/(30*100)


####Task 4ii####
dfBlade_Weight<- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Blade Weight", skip = 2)
sample_mean = c()
for (i in (1:350)){ 
  average = mean(dfBlade_Weight[sample(1:nrow(dfBlade_Weight), 30, replace = TRUE),]$Weight)
  sample_mean = c(sample_mean, c(average))
}
hist(sample_mean)
mean(sample_mean)
mean(dfBlade_Weight$Weight)
BW.sd<-sd(dfBlade_Weight$Weight)
BW.sd/sqrt(350) #standard error
shapiro.test(sample_mean)


####Task 4iii####
hlines<-as.matrix(quantile(dfBlade_Weight$Weight, probs=c(0.25,0.75)))
bwiqr<-IQR(dfBlade_Weight$Weight)
hlines<-rbind(hlines, matrix(c(hlines[1,1] - 1.5*bwiqr, hlines[1,1] - 3*bwiqr, hlines[2,1] + 1.5*bwiqr, hlines[2,1] + 3*bwiqr), nrow=4, ncol=1))
hlines<-cbind(hlines, c('green','green','blue','red','blue','red'))

plot(dfBlade_Weight, main="Blade Weights")
abline(h=hlines[,1], col=hlines[,2], lty=2)
legend('topright', legend=c('1st, 3rd Quartile','1.5 x IQR','3 x IQR'), fill = c( 'green', 'blue', 'red'))



####Task 5i####
dfOn_Time_Delivery <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "On-Time Delivery", skip = 2)
plot(dfOn_Time_Delivery$Month, dfOn_Time_Delivery$Percent, type='o', main='Proportion of On-Time deliveries over the months', ylab='Proportion of On-Time deliveries', xlab='Year')
t.test(dfOn_Time_Delivery$Percent[1:12], dfOn_Time_Delivery$Percent[49:60], alternative='less')


####Task 5ii####
dfDefects <- read_excel("Performance Lawn Equipment Database.xlsx", sheet = "Defects After Delivery", skip = 3)
plot(c(dfDefects$`2010`, dfDefects$`2011`, dfDefects$`2012`, dfDefects$`2013`, dfDefects$`2014`), type='o', main='Defects after delivery over the months', ylab='Defects after delivery', xlab='Year', xaxt="n")
axis(1, at = c(1,13,25,37,49), labels = 2010:2014)
t.test(dfDefects$`2010`, dfDefects$'2014', alternative='two.sided')