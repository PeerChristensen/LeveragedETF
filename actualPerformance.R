#compare actual (leveraged) ETFs with theoretical performance, plots and correlation

years=2010:2017
dfList=list()
counter=1
for (i in years){
  getSymbols(c("^GSPC","UPRO","SPXL"),from=paste0(i,"-01-01"),to=paste0(i,"-12-31"),
             src="yahoo",class=ts)
  
  df=data.frame(date=index(GSPC),
                cumsum(dailyReturn(GSPC)),
                cumsum(dailyReturn(UPRO)),
                cumsum(dailyReturn(SPXL)),
                cumsum(dailyReturn(GSPC))*3)
  names(df) = c("date","SP500","UPRO","SPXL","X3")
  df <- melt(df,id="date")
  dfList[[counter]] = df
  counter = counter +1
}

#yearly lines
dfConcat=rbindlist(dfList)
dfConcat$year=year(dfConcat$date)
dfConcat$Day=yday(dfConcat$date) #julian day
ggplot(dfConcat, aes(x=Day,y=value)) + geom_line(aes(colour = variable)) +
  facet_wrap(~year,ncol=3)

#annual return bars
annReturn=ddply(dfConcat, .(variable,year), function(x) x[nrow(x), ])
annRetunBars=ggplot(annReturn,aes(x=variable,y=value)) +
  geom_bar(stat="identity",aes(fill=variable))+
  facet_wrap(~year,ncol=2)

