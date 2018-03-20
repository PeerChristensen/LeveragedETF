
#Leveraged ETFs

if(!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod,ggplot2,reshape,lubridate)

#SP500: 2015-2018
getSymbols(c("^GSPC"),from="2013-01-01",to="2018-01-01",src="yahoo",class=ts)
SP=GSPC[,6]
diff=diff(SP)
dayChangePCT=diff/SP*100
df=data.frame(SP,diff,dayChangePCT)
names(df)=c("SP","diff","dayChangePCT")
df$dayChangePCT[1]=0
df$cumChangePCT=cumsum(df$dayChangePCT)
x1=df$cumChangePCT
dfLev=data.frame(date=as.Date(row.names(df)),x1,
                 x3=x1*3,
                 x5=x1*5,
                 x10=x1*10,
                 x15=x1*15,
                 x20=x1*20)

dfLevMelt <- melt(dfLev,id="date")
ggplot(dfLevMelt, aes(x=date,y=value)) + geom_line(aes(colour = variable))

dfLevMelt$Year=year(dfLevMelt$date)
dfLevMelt$Day=yday(dfLevMelt$date) #julian day
ggplot(dfLevMelt, aes(x=Day,y=value)) + geom_line(aes(colour = variable)) +
  facet_wrap(~Year,ncol=3)
                                                
ggplot(dfLevMelt,aes(x=variable,value)) +
  geom_bar(stat="identity",fill="gray",colour="#A0B876")
  

############## another way
getSymbols(c("^GSPC"),from="2008-01-01",to="2018-01-01",src="yahoo",class=ts)
x1=data.frame(cumsum(dailyReturn(GSPC)))
dfLev2=data.frame(date=as.Date(row.names(x1)),x1,
                 x3=x1*3,
                 x5=x1*5,
                 x10=x1*10,
                 x15=x1*15,
                 x20=x1*20)
names(dfLev2) = c("date","x1","x3","x5","x10","x15","x20")
dfLevMelt2 <- melt(dfLev2,id="date")

ggplot(dfLevMelt2, aes(x=date,y=value)) + geom_line(aes(colour = variable))

dfLevMelt2$Year=year(dfLevMelt2$date)
dfLevMelt2$Day=yday(dfLevMelt2$date) #julian day
ggplot(dfLevMelt2, aes(x=Day,y=value)) + geom_line(aes(colour = variable)) +
  facet_wrap(~Year,ncol=3)

ggplot(dfLevMelt2,aes(x=variable,value)) +
  geom_bar(stat="identity",fill="gray",colour="#A0B876")+
  facet_wrap(~Year,ncol=3)

################################
getSymbols(c("TQQQ"),from="2017-01-01",to="2018-01-01",src="yahoo",class=ts)
TQQQ=TQQQ[,6]
diff=diff(TQQQ)
dayChangePCT=diff/TQQQ*100
df=data.frame(TQQQ,diff,dayChangePCT)
names(df)=c("TQQQ","diff","dayChangePCT")
df$dayChangePCT[1]=0
df$cumChangePCT=cumsum(df$dayChangePCT)
x1=df$cumChangePCT
dfLev=data.frame(date=as.Date(row.names(df)),
                 # x2=x1*2,
                 x3=x1*3,
                 x1)



a=ddply(dfLev, "Year", summarise)


