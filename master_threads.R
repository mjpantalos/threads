#======
# Determine specimen label
#======
# Make sure working directory is set for .csv file. 
# If cloning directly from github, .csv file is already in working directory.

data_1 <- read.csv(file = "Specimen_RawData_7.csv", header = TRUE,
                   stringsAsFactors = FALSE, skip = 0)
head(data_1)
specimen_label <- data_1[1,1] 

data_1 <- read.csv(file = "Specimen_RawData_7.csv", header = TRUE, stringsAsFactors = FALSE, skip = 5)
names(data_1)<-c("time", "extension", "load")

#=====
# Define Functions
#=====

#LowRes: reduce the number of points by taking mean
LowRes<-function(load_data, dx=1){
 #find the number of points in new dataset having dx milimeters between each point
 numnewpoints<-trunc(max(load_data$extension)/dx)
 #create dataframe for point reduction
 newpoints<-data.frame(extension=rep(0, numnewpoints), load=rep(0, numnewpoints))
 #number of points in interval dx
 numpoints<-length(load_data$extension)/numnewpoints
 #defining needed variables for loop
 extenmean<-NULL
 loadmean<-NULL
 #loop for processing remaining data
 for(i in 1:numnewpoints){
  #finding mean extension during interval i
  extenmean<-mean(load_data$extension[((i-1)*numpoints+1):(i*numpoints)])
  #finding mean load during interval i
  loadmean<-mean(load_data$load[((i-1)*numpoints+1):(i*numpoints)])
  #adding mean extension and load for interval i to newpoints
  newpoints$extension[i]<-extenmean
  newpoints$load[i]<-loadmean
 }
 return(newpoints)
}



#FindPeaks: finds points with preceding slope greater than PreS and following slope less than PostS
FindPeaks<-function(load_data, PreS=0, PostS=-1){
  x1<-NULL
  x2<-NULL
  x3<-NULL
  y1<-NULL
  y2<-NULL
  y3<-NULL
  s1<-NULL
  s2<-NULL
  peaks<-data.frame(extension=c(0), load=c(0))
  #loop for each point, except first and last
  for(i in 2:(length(load_data$extension)-1)){
    #extention of preceding point
    x1<-load_data$extension[i-1]
    #extension of point being considered
    x2<-load_data$extension[i]
    #extension of following point
    x3<-load_data$extension[i+1]
    #load of preceding point
    y1<-load_data$load[i-1]
    #load of point being considered
    y2<-load_data$load[i]
    #load of following point
    y3<-load_data$load[i+1]
    #precedding slope
    s1<-(y2-y1)/(x2-x1)
    #following slope
    s2<-(y3-y2)/(x3-x2)
    #test if matches specified slope requirements
    if (s1>PreS & s2<PostS){
      #for if the point is the first being considered
      if(peaks$load[1]==0){
        peaks$extension[1]<-x2
        peaks$load[1]<-y2
      #for all points being considered except the first, adds to existing points
      }else{
        peaks<-rbind(peaks, data.frame(extension=x2, load=y2))
      }
    }
  }
  return(peaks)
}


data_1_low<-LowRes(load_data=data_1, dx=0.05)
data_1_low_peaks<-FindPeaks(load_data=data_1_low, PreS=0, PostS=-0.5)

plot(data_1$load~data_1$extension, pch=".")
points(data_1_low, pch=16, cex=0.5)
points(data_1_low_peaks, pch=16, cex=1)
