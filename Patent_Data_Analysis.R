
PatentData3 <- read.csv("/Users/sakethsaripalli/RProjects/PatentData2.csv")
length(PatentData3$U.S..Regional.Level)
tempV <- PatentData2[1, ]

PatentData3 <- PatentData2

PatentData3<- PatentData3[-1, ]

colnames(PatentData3) <- tempV

View(PatentData3)
colnames(PatentData3)
length(PatentData3$`U.S. Regional Level`)

PatentData3 <- PatentData3[1:376, ]
View(PatentData3)
length(PatentData3$V1)
length(PatentData3$`U.S. Regional Level`)
length(MSA_Populations$V1)


PatentData3 <- cbind(PatentData3, cframe)
PatentData3 <- PatentData3[, -(27:32)]
View(PatentData3)
length(PatentData3)

library(taRifx)
library(ggplot2)

PatentSums <- c(sum(PatentData3$X2010, na.rm = TRUE), sum(PatentData3$X2011, na.rm = TRUE), sum(PatentData3$X2012, na.rm = TRUE), sum(PatentData3$X2013, na.rm = TRUE), sum(PatentData3$X2014, na.rm = TRUE), sum(PatentData3$X2015, na.rm =TRUE)) 
PopulationSums <- c(sum(as.numeric(PatentData3$`2010`), na.rm = TRUE), sum(as.numeric(PatentData3$`2011`), na.rm = TRUE), sum(as.numeric(PatentData3$`2012`), na.rm = TRUE), sum(as.numeric(PatentData3$`2013`), na.rm = TRUE), sum(as.numeric(PatentData3$`2014`), na.rm = TRUE), sum(as.numeric(PatentData3$`2015`), na.rm = TRUE))

PatentTotals <- data.frame(PopulationSums, PatentSums)
plotTotals <- ggplot(PatentTotals, aes(y = PatentSums, x= PopulationSums)) + geom_smooth() + geom_point() + xlab("Population Totals") + ylab("Patent Sums") + ggtitle("Population vs Patent Totals") 
cor(PatentTotals$PopulationSums, PatentTotals$PatentSums)


View(GDPData)


k <- PatentData3[, "2010"]
r <- PatentData3$X2010

PatentData3 <- remove.factors(PatentData3)

Patents2011 <- as.numeric(PatentData3$X2011)
Population2011 <- as.numeric(PatentData3$`2011`)


Patents2012 <- as.numeric(PatentData3$X2012)
Populations2012 <- as.numeric(PatentData3$`2012`)

Patents2013 <- as.numeric(PatentData3$X2013)
Population2013 <- as.numeric(PatentData3$`2013`)

Patents2014 <- as.numeric(PatentData3$X2014)
Population2014 <- as.numeric(PatentData3$`2014`)

Patents2015 <- as.numeric(PatentData3$X2015)
Population2015 <- as.numeric(PatentData3$`2015`)


PatentFrame2011 <- data.frame(Population2011, Patents2011)
PatentFrame2011 <- remove.factors(PatentFrame2011)


PatentFrame2011 <- subset(PatentFrame2011, !is.na(PatentFrame2011$Population2011))
PatentFrame2011 <- subset(PatentFrame2011, !is.na(PatentFrame2011$Patents2011))

PatentFrame2012 <- data.frame(Populations2012, Patents2012)
PatentFrame2012 <- remove.factors(PatentFrame2012)

PatentFrame2012 <- subset(PatentFrame2012, !is.na(PatentFrame2012$Populations2012))
PatentFrame2012 <- subset(PatentFrame2012, !is.na(PatentFrame2012$Patents2012))

PatentFrame2013 <- data.frame(Population2013, Patents2013)
PatentFrame2013 <- remove.factors(PatentFrame2013)

PatentFrame2013 <- subset(PatentFrame2013, !is.na(PatentFrame2013$Population2013))
PatentFrame2013 <- subset(PatentFrame2013, !is.na(PatentFrame2013$Patents2013))

PatentFrame2014 <- data.frame(Population2014, Patents2014)
PatentFrame2014 <- remove.factors(PatentFrame2014)

PatentFrame2014 <- subset(PatentFrame2014, !is.na(PatentFrame2014$Population2014))
PatentFrame2014 <- subset(PatentFrame2014, !is.na(PatentFrame2014$Patents2014))

PatentFrame2015 <- data.frame(Population2015, Patents2015)
PatentFrame2015 <- remove.factors(PatentFrame2015)

PatentFrame2015 <- subset(PatentFrame2015, !is.na(PatentFrame2015$Population2015))
PatentFrame2015 <- subset(PatentFrame2015, !is.na(PatentFrame2015$Patents2015))

Patents2010 <- data.frame(r, k)

colnames(Patents2010) <- c("Patents", "Population")
View(plotted2010)
class(plotted2010$Patents)
class(plotted2010)

fplotted2010 <- data.frame(as.numeric(plotted2010$Population), as.numeric(plotted2010$Patents))
View(fplotted2010)

colnames(fplotted2010) <- c("Population", "Patents")

pl2010 <- ggplot(fplotted2010, aes(y = Patents, x = Population)) + geom_point() + scale_x_continuous(name = "Population", limits = c(0, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4893", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.201", parse=T) + ggtitle("Population vs Patents 2010")
pl2011 <- ggplot(PatentFrame2011, aes(x = Population2011, y = Patents2011)) + geom_point() + scale_x_continuous(name = "Population", limits = c(85000, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4639", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.196", parse=T) + ggtitle("Population vs Patents 2011")
pl2012 <- ggplot(PatentFrame2012, aes(x = Populations2012, y = Patents2012)) + geom_point() + scale_x_continuous(name = "Population", limits = c(85000, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4725", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.205", parse=T) + ggtitle("Population vs Patents 2012")
pl2013 <- ggplot(PatentFrame2013, aes(x = Population2013, y = Patents2013)) + geom_point() + scale_x_continuous(name = "Population", limits = c(85000, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4722", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.186", parse=T) + ggtitle("Population vs Patents 2013")
pl2014 <- ggplot(PatentFrame2014, aes(x = Population2014, y = Patents2014)) + geom_point() + scale_x_continuous(name = "Population", limits = c(85000, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4877", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.215", parse=T) + ggtitle("Population vs Patents 2014")
pl2015 <- ggplot(PatentFrame2015, aes(x = Population2015, y = Patents2015)) + geom_point() + scale_x_continuous(name = "Population", limits = c(85000, 1000000)) + scale_y_continuous(name = "Patents", limits = c(0, 1000)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE) +  annotate("text", x=10000000, y=85, label = "R^2 == 0.4743", parse=T) + annotate("text", x=10000000, y=40, label = "beta == 1.169", parse=T) + ggtitle("Population vs Patents 2015")



par(mfrow = c(2, 3))
plot(pl2010)
plot(pl2011)

max(PatentFrame2011$Population2011)

PatentFrame2011$

plot(pl2010)
logY <- log10(fplotted2010$Patents)[1:(length(logY) - 3)]
logX <- log10(fplotted2010$Population)[1:(length(logX) - 3)]

logY2 <- log10(PatentFrame2011$Patents2011)[-c(324, 360, 367)]
infIndexes2011 <- which(logY2 == -Inf) 
logX2 <- log10(PatentFrame2011$Population2011)[-c(324, 360, 367)]


logY3 <- log10(PatentFrame2012$Patents2012)[-c(345, 366)]
which(logY3 == -Inf)
logX3 <- log10(PatentFrame2012$Populations2012)[-c(345,366)]

logY4 <- log10(PatentFrame2013$Patents2013)[-c(336, 365, 366, 367, 368)]
which(logY4 == -Inf)
logX4 <- log10(PatentFrame2013$Population2013)[-c(336, 365, 366, 367, 368)]

logY5 <- log10(PatentFrame2014$Patents2014)[-c(358, 365, 367)]
which(logY5 == -Inf)
logX5 <- log10(PatentFrame2014$Population2014)[-c(358, 365, 367)]

logY6 <- log10(PatentFrame2015$Patents2015)[-c(352, 358, 365, 366, 367, 368)]
which(logY6 == -Inf)
logX6 <- log10(PatentFrame2015$Population2015)[-c(352, 358, 365, 366, 367, 368)]


length(logY6)
length(logX6)

summary()

length(logY2)
length(logX2)


logY[length(logY)]
print(linear2010)
print(linear2011)


linear2010 <- lm(formula = logY ~ logX)
linear2011 <- lm(formula = logY2 ~ logX2)
linear2012 <- lm(formula = logY3 ~ logX3)
linear2013 <- lm(formula = logY4 ~ logX4)
linear2014 <- lm(formula = logY5 ~ logX5)
linear2015 <- lm(formula = logY6 ~ logX6)

summary(linear2010)
summary(linear2011)
summary(linear2012)
summary(linear2013)
summary(linear2014)
summary(linear2015)

print(linear2012)
print(linear2013)
print(linear2014)
print(linear2015)
linear2010$

sum(is.na(fplotted2010$Patents))




View(plotted2010)
plot(plotted2010$Patents, plotted2010$Population)


plot(pl)
max(Patents2010$Patents)
length(Patents2010$Patents)

sum(is.na(plot2010))
plot2010 <- subset(Patents2010, !is.na(Patents2010$Population))
plot2010 <- subset(plot2010, !is.na(plot2010$Patents))
length(plot2010$Patents)

max(plotted2010$Patents)
min(plotted2010$Patents)
library(taRifx)
plotted2010 <- remove.factors(plot2010)

max(plotted2010$Population)
min(plotted2010$Population)







ggplot()




View(GDPData)


tvec <- MSA_Populations[2, ]



colnames(MSA_Populations) <- tvec
View(MSA_Populations)





colnames(GDPData) <- c("Names", "2011", "2012", "2013", "2014", "2015", "2016", "Totals")
GDPData <- GDPData[-1, -length(GDPData)]


tp <- c(TRUE, FALSE, TRUE)
which(tp == TRUE)

teVec <- c()


for(i in 1 : length(MSA_Populations$`United States - Akron, OH Metro Area`)){
  
  ts <- substr(MSA_Populations$`United States - Akron, OH Metro Area`[i] , 17, str_length(MSA_Populations$`United States - Akron, OH Metro Area`[i]))
  teVec <- c(teVec, ts)  
  
  
  
  
}
MSA_Populations <- read.csv("/Users/sakethsaripalli/RProjects/MSA_Populations.csv")

MSA_Populations <- MSA_Populations[-(1:3), ]
View(MSA_Populations)
colnames(MSA_Populations) <- c("Geography", "Census", "Estimates.Base", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

PatentData3 <- cbind(PatentData3, 1:length(PatentData3$`U.S. Regional Level`))
tempDat <- data.frame(1:length(PatentData3$`U.S. Regional Level`))
length(PatentData3)
View(PatentData3)
colnames(PatentData3[21:26]) <- c("Pop2010", "Pop2011", "Pop2012", "Pop2013", "Pop2014", "Pop2015")
View(MSA_Populations)
library(stringr)
matched <- data.frame()
cVec <- c()
for(i in 1 :length(PatentData3$`U.S. Regional Level`)){
  
  t <- word(PatentData3$`U.S. Regional Title`[i], 1, sep = "-")
  t2 <- word(t, 1, sep = ",")
  print(t2)
  cVec <- c(cVec, t2)
  
  
  
  
  
  #View(MSA_Populations[43:46, ])
  
  #matched <- rbind(matched, MSA_Populations[grep("Boston", MSA_Populations$`United States - Akron, OH Metro Area`), ])
  
  
}
View(MSA_Populations)

cframe <- data.frame()
View(cframe)
length(cframe$`2010`)
for(i in cVec){
  
  
  
  f <- grep(i, MSA_Populations[, "Census"])
  
  tcVec <- MSA_Populations[f[1], 4:9]
  print(tcVec)
  
  cframe <- rbind(cframe, tcVec)
  #PatentData3[i, 21:26] <- tcVec
  
  
  
  
}
View(PatentData3)
length(PatentData3$V1)
PatentData3 <- PatentData[, -(21:26)]
PatentData3 <- cbind(PatentData3, cframe)



View(PatentData3[, 21:26])


View(PatentData3)

MSA_Populations[grep("Boston", MSA_Populations[, "Census"])[1], 4:9]

View(cVec)
grep(cVec[5], MSA_Populations[, "Census"])
View(cVec)
teVec

grep("Phoenix", c("Boston", "Phoenix", "Atlanta"))

str_detect(c("Boston-Newton", "Bosto-Newton"), "Boston")


pmatch("Boston", "Boston-Newton")


length(GDPData$Names)
