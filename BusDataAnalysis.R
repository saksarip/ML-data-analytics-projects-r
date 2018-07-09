

Bus_Data = read.csv("/Users/sakethsaripalli/RProjects/Bus_Breakdown_Data_CSV.csv", stringsAsFactors = FALSE)
summary(Bus_Data)
unique(Bus_Data$School_Year)
unique(Bus_Data$Route_Number)
length(unique(Bus_Data$Reason))
uniqueR <- as.character(unique(Bus_Data$Route_Number))
uniqueC <- as.character(unique(Bus_Data$Reason))
finalMatrix <- matrix(0, length(uniqueR), length(uniqueC))
View(Bus_Data)

k<-1
m<-1
counter <- 0
vec1
for(i in 1: length(uniqueR)){
  
  
  for(j in 1 : length(uniqueC)){
    
    temp = subset(Bus_Data, (Bus_Data$Route_Number == uniqueR[i] & Bus_Data$Reason == uniqueC[j]))
    finalMatrix[k,m] = length(temp$Reason)
    
    
    
    
    
    
    
    m <- m + 1
    counter <- counter + 1
    print(counter)
  }
  k <- k + 1
  m <- 1
}


finalFrame <- as.data.frame(finalMatrix)
finalFrame <- finalFrame[, -11]
rownames(finalFrame) <- uniqueR
colnames(finalFrame) <- uniqueC[1:10]

write.csv(finalFrame, "Bus_Stoppages_By_Route")


busstop.Calculator <- function(route){
  
  sum <- sum(finalFrame[route, ])
  vec1 <- finalFrame[route, ] / sum
 
  return(vec1) 
  
  
}



tempVec 
percentageFrame
percentageFrame <- data.frame()
length(rownames(finalFrame))






rs <- rowSums(finalFrame)
percentageFrame <- finalFrame / rs




failureFrame <- finalFrame[, c()]


ggplot()




  
  






