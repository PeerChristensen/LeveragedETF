#Simulations of stock price movement and annual return 
library(reshape2)
library(ggplot2)
library(gridExtra)

# Stochastic process stock simulation in R for stock X
#https://www.r-bloggers.com/stochastic-processes-and-stocks-simulation/

set.seed(18293)
dfList=list()
for (i in 1:1000){
  Z <- rnorm(255,0,1)   # Random normally distributed values, mean = 0, stdv = 1
  returnAnn <- 0.1              # Expected annual return (30%)
  sd <- 0.1             # Expected annual standard deviation (20%)
  start <- 100              # Starting price
  price <- c(start)         # Price vector
  count <- 2                # See below
  days <- 1:256        # Time. Days to put on the x axis

  for(j in Z){
    S = start + start*(returnAnn/255 + sd/sqrt(255)*j)
    price[count] <- S
    start = S
    count = count + 1
    }

  price=ts(price)
  diff=c(price[1],diff(price))
  dayChangePCT=diff/price*100
  dayChangePCT[1]=0
  x1=cumsum(dayChangePCT)
  dfList[[i]]=data.frame(days,x1,
                 x3=x1*3,
                 x5=x1*5,
                 x10=x1*10,
                 x15=x1*15,
                 x20=x1*20)
}

df=data.frame()
for(i in dfList){
  df=rbind(df,i[256,])
}

#plot a random selection of simulations
plotList=list()
pickedDFs = sample(1:1000,6)
for (i in pickedDFs){
  melted=melt(dfList[[i]],id="days")
  name=paste(i)
  plotList[[name]]=ggplot(melted, aes(x=days,y=value)) + geom_line(aes(colour = variable))
}
multiplot(plotlist=plotList,cols=2)

# barchart of simulated mean annual returns
dfLong=melt(df[,-1])
dfAgg <- ddply(dfLong, c("variable"), summarise,
               mean=mean(value),sd=sd(value))

ggplot(dfAgg, aes(x=variable, y=mean,fill=variable)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="gray30") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9))






plot(days,price,main="Time series stock X",xlab="time",ylab="price", type="l",col="blue")
summary(price)
statistics<- c(sd(price),mean(price),(price[256]-price[1])/price[1]*100)
names(statistics) <- c("Volatility","Average price","Return %")
print(statistics)