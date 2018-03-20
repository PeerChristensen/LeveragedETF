#tivoli, does stock go up before spring/summer season?

getSymbols(c("TIV.CO"),from="2010-01-01",to="2018-01-01",src="yahoo",class=ts)
t=TIV.CO[,6]
plot(t)

#this looks weird, let's split up the data
getSymbols(c("TIV.CO"),from="2011-01-01",to="2014-12-31",src="yahoo",class=ts)
t1=TIV.CO[,6]
plot(t1)

getSymbols(c("TIV.CO"),from="2015-01-01",to="2017-12-31",src="yahoo",class=ts)
t2=TIV.CO[,6]
plot(t2)

returnsByMonth=periodReturn(TIV.CO,period='monthly')
plot(returnsByMonth)

df=as.data.frame(returnsByMonth)
#note that month.name and month.abb are constants built into base R
df$Month=factor(rep(month.abb,length(returnsByMonth)/12),levels = month.abb)
head(df)

meanReturns=ddply(df,"Month",summarize,
                  Mean=mean(monthly.returns),
                  SE=sd(monthly.returns)/sqrt(length(monthly.returns)))
meanReturns

meanReturnsBar=ggplot(meanReturns, aes(x=Month, y=Mean))+
  geom_bar(stat="identity",fill="#E69F00",colour="gray30")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9))+
  ggtitle("Tivoliaktiens månedlige afkast 2015-2018")+
  theme(plot.title=element_text(lineheight=0.8,hjust=0,face="bold", size=16),
        axis.title.x=element_text(face="bold",size=14), 
        #axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=14),
        axis.text.y=element_text(size=15),
        legend.position = "none")
meanReturnsBar