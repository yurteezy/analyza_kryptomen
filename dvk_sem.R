library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(plotly)
install.packages("rmarkdown")

#data
sp500 <- read.csv('/Users/appletrh/Desktop/VSE study materials/S&P 500 Historical Data.csv')
btc <- read.csv('/Users/appletrh/Desktop/VSE study materials/BTC_USD Bitfinex Historical Data.csv')
nsdq <- read.csv('/Users/appletrh/Desktop/VSE study materials/NASDAQ Composite Historical Data.csv')
gold <- read.csv('/Users/appletrh/Desktop/VSE study materials/Gold Futures Historical Data.csv')
btc <- btc[-c(1),]

#number format
btc$Change <- as.numeric(sub("%","",btc$Change))/100
btc$Price <- gsub(",", "", btc$Price)
btc$Price <- as.numeric(btc$Price)
sp500$Price <- as.numeric(sub(",","",sp500$Price))
sp500$Change <- as.numeric(sub("%","",sp500$Change))/100

gold$Change <- as.numeric(sub("%","",gold$Change))/100
gold$Price <- as.numeric(sub(",","",gold$Price))
nsdq$Change <- as.numeric(sub("%","",nsdq$Change))/100

#date format
btc$Date <- as.Date(btc$Date, '%B %d, %Y')
sp500$Date <- as.Date(sp500$Date, '%B %d, %Y')
nsdq$Date <- as.Date(nsdq$Date, '%B %d, %Y')
gold$Date <- as.Date(gold$Date, '%B %d, %Y')

#correlation
a <- cor(btc$Change, sp500$Change)
b <- cor(btc$Change, nsdq$Change)
c <- cor(btc$Change, gold$Change)


dd <- cor(btc22$Change,sp22$Change)




#correlation graphs
corelace1 <- data.frame(btc$Change,sp500$Change,btc$Date)
colnames(corelace1) <- (c('btc','sp500','date'))

btcsp500 <- ggplot(corelace1, aes(x=btc, y=sp500)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  theme(axis.text = element_text( 
    color="darkgreen", 
    size=15, 
    face=3)
  )
btcsp500 <- btcsp500 + labs(title = ("Bitcoin/SP 500 correlation from 2016 till now"),
                x = ("Bitcoin"), y = ("SP 500"))
btcsp500 <- btcsp500 + theme(
  plot.title = element_text(color="darkslategrey", size=16, face="bold.italic"),
  axis.title.x = element_text(color="darkseagreen", size=14, face="bold"),
  axis.title.y = element_text(color="darkseagreen", size=14, face="bold")) 
btcsp500 + annotate('text',label=("Correlation:"), x = -0.3, y=0.1) +
  annotate('text', label = (round(a,2)), x = -0.17, y=0.1,
           col = "red",size = 5)



#data for 2022 correlation
btc22 <- subset(btc, Date > "2021-12-31")
sp22 <- subset(sp500, Date > "2021-12-31")
nsdq22 <- subset(nsdq, Date > "2021-12-31")
gld22 <- subset(gold, Date > "2021-12-31")
cordata <- data.frame(nsdq22$Change,btc22$Change)
colnames(cordata) <- (c('btc','nasdaq'))
nasbtc <- cor(btc22$Change,nsdq22$Change)


##next 3
corelace2 <- data.frame(btc22$Change,sp22$Change)
colnames(corelace2) <- (c('btc2','sp5002'))

btcsp2 <- ggplot(corelace2, aes(x=btc2, y=sp5002)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  theme(axis.text = element_text( 
    color="darkgreen", 
    size=15, 
    face=3)
  )
btcsp2 <- btcsp2 + labs(title = ("Bitcoin/SP 500 correlation in 2022"),
                            x = ("Bitcoin"), y = ("SP 500"))
btcsp2 <- btcsp2 + theme(
  plot.title = element_text(color="darkslategrey", size=16, face="bold.italic"),
  axis.title.x = element_text(color="darkseagreen", size=14, face="bold"),
  axis.title.y = element_text(color="darkseagreen", size=14, face="bold")) 
btcsp2 + annotate('text',label=("Correlation:"), x = -0.15, y=0.1) +
  annotate('text', label = (round(dd,2)), x = -0.10, y=0.1,
           col = "red",size = 5)



## bitcoin nasdaq history
hoh <- data_frame(btc$Change,nsdq$Change)
colnames(hoh) <- (c('btc','nasdaq'))
qwer <- ggplot(hoh,aes(x=btc, y=nasdaq)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  theme(axis.text = element_text( 
    color="darkgreen", 
    size=15, 
    face=3)
  )

qwer <- qwer + labs(title = ("Bitcoin  Nasdaq correlation "),
                x = ("Bitcoin"), y = ("Nasdaq"))

qwer <- qwer + theme(
  plot.title = element_text(color="darkslategrey", size=16, face="bold.italic"),
  axis.title.x = element_text(color="darkseagreen", size=14, face="bold"),
  axis.title.y = element_text(color="darkseagreen", size=14, face="bold"))

qwer + annotate('text',label=("Correlation:"), x = -0.36, y=0.1) +
  annotate('text', label = (round(b,2)), x = -0.21, y=0.1,
           col = "red",size = 5)



#graph 1 bitcoin nasdaq cor 22
p3 <- ggplot(cordata, aes
             (x=btc, y=nasdaq)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  theme(axis.text = element_text( 
    color="darkgreen", 
    size=15, 
    face=3)
  )

p3 <- p3 + labs(title = ("Bitcoin/Nasdaq correlation in 2022"),
  x = ("Bitcoin"), y = ("Nasdaq"))

p3 <- p3 + theme(
  plot.title = element_text(color="darkslategrey", size=16, face="bold.italic"),
  axis.title.x = element_text(color="darkseagreen", size=14, face="bold"),
  axis.title.y = element_text(color="darkseagreen", size=14, face="bold"))
  
p3 + annotate('text',label=("Correlation:"), x = -0.06, y=0.1) +
  annotate('text', label = (round(nasbtc,2)), x = -0.034, y=0.1,
           col = "red",size = 5)
 
#spojnicovy\


ggplot(btc , aes(x=Date, y=Price))+ 
  geom_line(color="deeppink",show.legend=TRUE)+
  geom_line(aes(y=sp500$Price),color="blue2",show.legend=TRUE)+
  scale_y_continuous(trans="log2") +
  theme(
    panel.background = element_rect(fill = "aliceblue", colour="cornflowerblue"),
    panel.grid.minor = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "white",),
  )+
  ggtitle("Bitcoin and SP500 ")
    o        
  cor()
  
  
  
  
#GOLD
c
data <- data_frame(btc$Change,gold$Change)
colnames(data) <- (c('btc','gold'))
zlato <- ggplot(data, aes(x=btc, y=gold)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  theme(axis.text = element_text( 
    color="darkgreen", 
    size=15, 
    face=3)
  )

zlato <- zlato + labs(title = ("Bitcoin/gold correlation"),
                x = ("Bitcoin"), y = ("Nasdaq"))
zlato <- zlato + theme(
  plot.title = element_text(color="darkslategrey", size=16, face="bold.italic"),
  axis.title.x = element_text(color="darkseagreen", size=14, face="bold"),
  axis.title.y = element_text(color="darkseagreen", size=14, face="bold"))
zlato + annotate('text',label=("Correlation:"), x = -0.3, y=0.1) +
  annotate('text', label = (round(c,2)), x = -0.15, y=0.1,
           col = "red",size = 5)

