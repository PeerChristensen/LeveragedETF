#Show actual annual performance of indices and multiples
library(data.table)
library(quantmod)
library(reshape2)
library(ggplot2)
library(plyr)
library(plotly)

#SP500 2008-2018
years=2008:2017
dfList=list()
plotList=list()
counter=1
for (i in years){
  getSymbols(c("^GSPC"),from=paste0(i,"-01-01"),to=paste0(i,"-12-31"),src="yahoo",class=ts)
  x1=data.frame(cumsum(dailyReturn(GSPC)))
  df=data.frame(date=as.Date(row.names(x1)),x1,
                  x3=x1*3,
                  x5=x1*5,
                  x10=x1*10,
                  x15=x1*15,
                  x20=x1*20)
  names(df) = c("date","x1","x3","x5","x10","x15","x20")
  df <- melt(df,id="date")
  dfList[[counter]] = df
  plotList[[counter]]=ggplot(df, aes(x=date,y=value)) + geom_line(aes(colour = variable))+
    theme(axis.title.x=element_blank())
  counter = counter +1
}

#accuracy of the method
getSymbols(c("^GSPC"),from="2008-01-01",to="2017-12-31",src="yahoo",class=ts)
df=data.frame(GSPC[,6],cumsum(dailyReturn(GSPC)))
df=scale(df)
plot(as.Date(row.names(df)),df[,1],type="l")
lines(as.Date(row.names(df)),df[,2],col="red")
legend(x="top",c("SP500","cumsum(dailyReturn())"),lwd=c(2,2),col=c("black","red"), y.intersp=1.5)
cor(df)

#one plot per year
#multiplot(plotlist=plotList,cols=2)

#using facet wrap
dfConcat=rbindlist(dfList)
dfConcat$year=year(dfConcat$date)
dfConcat$Day=yday(dfConcat$date) #julian day
ggplot(dfConcat, aes(x=Day,y=value)) + geom_line(aes(colour = variable)) +
  facet_wrap(~year,ncol=3)

#bars annual return 
annReturn=ddply(dfConcat, .(variable,year), function(x) x[nrow(x), ])
annRetunBars=ggplot(annReturn,aes(x=variable,y=value)) +
  geom_bar(stat="identity",aes(fill=variable))+
  facet_wrap(~year,ncol=2)
#let's make it interactive with plotly
#note the empty plots, consider changing to ncol=2
ggplotly(annRetunBars)

