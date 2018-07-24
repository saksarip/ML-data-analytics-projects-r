View(GDPData)
length(GDPData$Names)
sum(is.na(GDPData))


length(GDPData$Names) == length(bframe$`2011`)
GDPData <- cbind(GDPData, bframe)

View(cGDPData)

class(cGDPData$`2011`)
sum(is.na(cGDPData))

cGDPData[, 1] <- GDPData$Names


cGDPData <- remove.factors(cGDPData)

for(i in 1 : length(cGDPData)){
  
  cGDPData[, i] <- as.numeric(gsub(",","",cGDPData[, i]))
  
  
}


View(cGDPData)
class(GDPData$`2011`)


View(GDPData)
GDPData <- read.csv("/Users/sakethsaripalli/RProjects/GDPData.csv")

colnames(GDPData) <- c("Names", "2011", "2012", "2013", "2014", "2015", "2016", "Totals")
GDPData <- GDPData[-1, -length(GDPData)]

colnames(GDPData) <- c("Names", "2011", "2012", "2013", "2014", "2015", "2016", "G2011", "G2012", "G2013", "G2014", "G2015", "G2016")


View()

vVec <- c()
for(i in 1 :length(GDPData$Names)){
  
  t <- word(GDPData$Names[i], 1, sep = "-")
  t2 <- word(t, 1, sep = ",")
  print(t2)
  vVec <- c(vVec, t2)
  
  }

bframe <- data.frame()

for(i in vVec){
  
  
  
  f <- grep(i, MSA_Populations[, "Census"])
  
  tcVec <- MSA_Populations[f[1], 5:10]
  print(tcVec)
  
  bframe <- rbind(bframe, tcVec)
  #PatentData3[i, 21:26] <- tcVec
  
  
  
  
}



View(GDPData)
sum(is.na(GDPData))

View(GDPData)
class(GDPData$`2011`)


library(taRifx)
length(GDPData$Names) == length(bframe$`2011`)



View(bframe)



GDPSums <- c(sum(cGDPData$`2011`), sum(cGDPData$`2012`), sum(cGDPData$`2013`), sum(cGDPData$`2014`), sum(cGDPData$`2015`), sum(cGDPData$`2016`))
GPopulationSums <- c(sum(cGDPData$G2011), sum(cGDPData$G2012), sum(cGDPData$G2013), sum(cGDPData$G2014), sum(cGDPData$G2015), sum(cGDPData$G2016))

GDPTotals <- data.frame(GPopulationSums, GDPSums)
plotTotals <- ggplot(GDPTotals, aes(y = GDPSums, x= GPopulationSums)) + geom_smooth() + geom_point() + xlab("Population Totals") + ylab("GDP Sums (thousands)") + ggtitle("GDP vs Population Totals") 
cor(GDPTotals$GPopulationSums, GDPTotals$GDPSums)



Pop2011 <- cGDPData$G2011
GDP2011 <- cGDPData$`2011`
GFrame2011 <- data.frame(Pop2011, GDP2011)

GLogY1 <- log10(GFrame2011$GDP2011)
GLogX1 <- log10(GFrame2011$Pop2011)
Glinear1 <- lm(GLogY1 ~ GLogX1)

gl2011 <- ggplot(GFrame2011, aes(y = GDP2011, x = Pop2011)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7863", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9771", parse=T) + ggtitle("Population vs GDP 2011") + xlab("Population") + ylab("GDP (thousands)")

Pop2012 <- cGDPData$G2012
GDP2012 <- cGDPData$`2012`
GFrame2012 <- data.frame(Pop2012, GDP2012)

GLogY2 <- log10(GFrame2012$GDP2012)
GLogX2 <- log10(GFrame2012$Pop2012)
Glinear2 <- lm(GLogY2 ~ GLogX2)

gl2012 <- ggplot(GFrame2012, aes(y = GDP2012, x = Pop2012)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7827", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9768", parse=T) + ggtitle("Population vs GDP 2012") + xlab("Population") + ylab("GDP (thousands)")

Pop2013 <- cGDPData$G2013
GDP2013 <- cGDPData$`2013`
GFrame2013 <- data.frame(Pop2013, GDP2013)

GLogY3 <- log10(GFrame2013$GDP2013)
GLogX3 <- log10(GFrame2013$Pop2013)
Glinear3 <- lm(GLogY3 ~ GLogX3)

gl2013 <- ggplot(GFrame2013, aes(y = GDP2013, x = Pop2013)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7842", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9805", parse=T) + ggtitle("Population vs GDP 2013") + xlab("Population") + ylab("GDP (thousands)")

Pop2014 <- cGDPData$G2014
GDP2014 <- cGDPData$`2014`
GFrame2014 <- data.frame(Pop2014, GDP2014)

GLogY4 <- log10(GFrame2014$GDP2014)
GLogX4 <- log10(GFrame2014$Pop2014)
Glinear4 <- lm(GLogY4 ~ GLogX4)

gl2014 <- ggplot(GFrame2014, aes(y = GDP2014, x = Pop2014)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7847", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9834", parse=T) + ggtitle("Population vs GDP 2014") + xlab("Population") + ylab("GDP (thousands)")

Pop2015 <- cGDPData$G2015
GDP2015 <- cGDPData$`2015`
GFrame2015 <- data.frame(Pop2015, GDP2015)

GLogY5 <- log10(GFrame2015$GDP2015)
GLogX5 <- log10(GFrame2015$Pop2015)
Glinear5 <- lm(GLogY5 ~ GLogX5)

gl2015 <- ggplot(GFrame2015, aes(y = GDP2015, x = Pop2015)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7872", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9890", parse=T) + ggtitle("Population vs GDP 2015") + xlab("Population") + ylab("GDP (thousands)")

Pop2016 <- cGDPData$G2016
GDP2016 <- cGDPData$`2016`
GFrame2016 <- data.frame(Pop2016, GDP2016)

GLogY6 <- log10(GFrame2016$GDP2016)
GLogX6 <- log10(GFrame2016$Pop2016)
Glinear6 <- lm(GLogY6 ~ GLogX6)

gl2016 <- ggplot(GFrame2016, aes(y = GDP2016, x = Pop2016)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(1000, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=1650, label = "R^2 == 0.7885", parse=T) + annotate("text", x=10000000, y=1045, label = "beta == 0.9920", parse=T) + ggtitle("Population vs GDP 2016") + xlab("Population") + ylab("GDP (thousands)")

