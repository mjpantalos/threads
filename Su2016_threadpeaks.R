#Initial version of a way to find peaks from summer 2016. Should check against values determined by hand. 



data_3 <- read.csv(file = "06142016/Specimen_RawData_7.csv", header = TRUE,
                       stringsAsFactors = FALSE, skip = 14)
names(data_3) <- c("time", "extension", "load")
head(data_3)

par(mfrow = c(1,3))
data <- data_3
plot(data$extension, data$load, pch ='.')


# Calculate thread strengths
par(mfrow = c(1,1))
diff <- matrix(data = NA, nrow = nrow(data)-2, ncol = 1)
data$load <- as.double(data$load)
for (i in 1:nrow(data)) {
  diff[i] <- data$load[i+1] - data$load[i]
}
dim(data$load)
length(diff)
plot(data$load)
points(data$load[diff<as.double(-0.01)], col = "red", pch = 20)
x <- which(diff<as.double(-0.01))-1
y <- na.exclude(data$load[diff<as.double(-0.01)])
length(x)
length(y)
points(x, y, col = "red", pch = 20)
plot(y)

data_ex <- y
data_ex <- as.double(data_ex)
str(data_ex)
diff_2 <- matrix(data = NA, nrow = length(data_ex)-1, ncol = 1)
for (i in 2:length(data_ex)-1) {
  diff_2[i] <- (data_ex[i+2]+data_ex[i+1])/2 - data_ex[i]
}
diff_2 <- na.exclude(diff_2)
x_2 <- which(diff_2>0.01)
y_2 <- diff_2[diff_2>0.01]
plot(x_2, y_2)

x_2 <- double(x_2)
y_2 <- double(y_2)

round(x_2/4)

thread_strength <- matrix (data = NA, nrow = 50, ncol = 2)
index <- 1
i<-1
while (i <= length(x_2)){
  if(x_2[i+1]==x_2[i]+1){
    if(y_2[i+1]>1*y_2[i] && y_2[i+1]<5*y_2[i]){
    thread_strength[index,1] <- x_2[i+1]
    thread_strength[index,2] <- y_2[i+1]
    index <- index+1
    i <- i+2
    print(i)
    } else {i <- i+1
    print(i)
    }
  } else {
    thread_strength[index,1] <- x_2[i]
    thread_strength[index,2] <- y_2[i]
    index <- index+1
    i <- i+1
    print(i)
    }
}
thread_strength

points(thread_strength, col = "red", pch = 20)
hist(thread_strength[,2])

par(mfrow = c(1,2))
plot(data$load, ylim = c(4,9))
points(x, y, col = "red", pch = 20)
points(jitter(rep(0, nrow(thread_strength)), amount = 250), 
       thread_strength[,2]+4)
hist(thread_strength[,2], main = "")

