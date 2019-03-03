#methods for plotting and comparing multiple stocks
library(quantmod)
years=2011:2018
par(mfrow=c(4,2))
for (i in years) {
  getSymbols(c("QQQ","TQQQ"),from=paste0(i,"-01-01"),to=paste0(i,"-12-31"),src="yahoo",class=ts)

  annualReturn(QQQ)
  annualReturn(TQQQ)

  QQQ1 <- as.numeric(QQQ$QQQ.Close[1])
  TQQQ1 <- as.numeric(TQQQ$TQQQ.Close[1])

  QQQ$QQQ <- QQQ$QQQ.Close/QQQ1
  TQQQ$TQQQ <- TQQQ$TQQQ.Close/TQQQ1

  basket <- cbind(QQQ$QQQ,TQQQ$TQQQ)
  zoo.basket <- as.zoo(basket)

  # Set a color scheme:
  #tsRainbow <- rainbow(ncol(zoo.basket))
  # Plot the overlayed series
  plot(x = zoo.basket, ylab = "Cumulative Return", main = paste(i),
    col = c("red","blue"), screens = 1)
  legend("topleft",names(zoo.basket),lwd=2,col=c("red","blue"))
  #plot(zoo.basket)
  #plot(zoo.basket,screens = 1)
  #plot(zoo.basket[,2],col="red",main=paste(i))
  #lines(zoo.basket[,1],col="blue")
  #legend("topleft",names(zoo.basket),lwd=2)
}