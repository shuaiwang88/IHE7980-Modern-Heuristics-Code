data <- read.table("26city.txt", quote="\"")
colnames(data) <- c("X", "Y")
data$index <- seq(1:nrow(data))

plot_TSP <- function(data, results) {
  lx <- min(data$X); rx <- max(data$X)
  ly <- min(data$Y); ry <- max(data$Y)
  
  plot(data$X, data$Y, type="p")
  text(data$X, data$Y, data$index, pos=4, cex=0.5)
  
  for(i in 1:(length(results)-1)) {
    segments(data$X[results[i]], data$Y[results[i]],
             data$X[results[i+1]], data$Y[results[i+1]])
    text((data$X[results[i]] +data$X[results[i+1]])/2,
         (data$Y[results[i]] +data$Y[results[i+1]])/2, "d")
  }
  segments(data$X[results[length(results)]], data$Y[results[length(results)]],
           data$X[results[1]], data$Y[results[1]])
}



cal_dist_matrix <- function(data) { 
  dist_matrix <- matrix(0, nrow(data), nrow(data));
  for (i in 1:nrow(data) ){
    for(j in 1:nrow(data)) {
      dist_matrix[i,j]<-sqrt((data$X[i]-data$X[j])^2+(data$Y[i]-data$Y[j])^2)
    }
  }
  
  return (dist_matrix)
}


data_matrix <-cal_dist_matrix(data)

object_eval <- function (data_matrix, solution)
{
  tour_distance <- 0;
  for(i in 1:(length(solution)-1)){
    tour_distance <- tour_distance + data_matrix[solution[i],solution[i+1]]
  }
  tour_distance <- tour_distance + data_matrix[solution[length(solution)],solution[1]]
  return(tour_distance)
}

result<-data_matrix
################

circular_before <- function(pos, nTotal) {
  if(pos==1) {
    prev <- nTotal } 
  else {
    prev <- pos-1 }
  return(prev)
}

circular_after<- function(pos, nTotal) {
  if(pos==nTotal) {
    after <- 1
  } else {
    after <-pos +1
  }
  return(after)
}
