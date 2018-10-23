data <- read.table("26city.txt", quote="\"", skip = 1)

# X <- c(4,2,8,0,1,5,7,3,6,5,8,1,2,3,6,0,8)
# Y <- c(4,0,0,1,1,2,2,3,3,5,5,6,6,7,7,8,8)
# 
# data <- data.frame(X,Y)
# data <- read.table("51.txt", quote="\"", skip = 1)

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


#####################euclidean distance#########################################
cal_dist_matrix <- function(data) {
  dist_matrix <- matrix(0, nrow(data), nrow(data));
  for (i in 1:nrow(data) ){
    for(j in 1:nrow(data)) {
      dist_matrix[i,j]<-sqrt((data$X[i]-data$X[j])^2+(data$Y[i]-data$Y[j])^2)
      # dist_matrix[i,j]<- abs(data$X[i]-data$X[j])+abs(data$Y[i]-data$Y[j])
      
    }
  }
  
  return (dist_matrix)
}





#cal_dist_matrix <- function(data) {
#  dist_matrix <- matrix(0, nrow(data), nrow(data));
#  for (i in 1:nrow(data)) {
#    for (j in 1:nrow(data)) {
#      dist_matrix[i,j] <- min(
#        (abs(data$Y[i] - up_endcap) + abs(data$X[i] - data$X[j]) +
#           abs(data$Y[j] - up_endcap)),
#        
#        (abs(data$Y[i] - low_endcap) + abs(data$X[i] - data$X[j]) +
#           abs(data$Y[j] - low_endcap)),
#        
#        (abs(data$Y[i] - mid_endcap) + abs(data$X[i] - data$X[j]) +
#           abs(data$Y[j] - mid_endcap))
#      )
#    }
#  }
#  return(dist_matrix)
#}



data_matrix <- cal_dist_matrix(data)

object_eval <- function (data_matrix, solution) {
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


# object_eval(data_matrix, seq(1:51))







# TABU search for TSP ##########################################################

## parameters:
max_iter <- 1000             # max iteration 
tabu_list_len <- 100         # length of tabu list
neighbor_num <- 30           # number of neighborhood to check at each iteration

tabu_list <- matrix(0, tabu_list_len, nrow(data)) # First in, first out


#########main part###################################################################
iter <- 1
cursolution <- sample(1:nrow(data))

while (iter <- max_iter){
  
  newsolution <- cursolution
  
  rand_pos1 <- round(runif(1,min=0.5,max=length(cursolution)+0.5))
  rand_pos2 <- round(runif(1,min=0.5,max=length(cursolution)+0.5))
  
  
  
  
  tmp <- newsolution[rand_pos1]
  newsolution[rand_pos1] <- newsolution[rand_pos2]
  newsolution[rand_pos2] <- tmp
  
  
}




































# THIS IS THE LIST THAT STORE THE SOLUTION PROCESS, IT IS A GLOBAL VARIABLE
# WITHIN THE FUNCITON, USE <<-  GLOBAL ASSIGNMENT, RATHER THAN <-
SA_V1<-function(temperature=500,t_min=0.0001,coolingRate=0.99)  {
  cursolution_list<-data.frame()
  
  cursolution <- data$index
  cursolution <- sample(cursolution)
  f1 <- object_eval(result, cursolution);
  
  best_solution <- cursolution
  best_solution_obj<-f1
  # print(best_solution_obj)
  #best_solution_obj2<-best_solution_obj
  nCities<-length(cursolution)
  index<-0
  while( temperature > t_min ){
    equiv_number <- length(cursolution)*(length(cursolution)-1)/2 
    for(i in 1:equiv_number)  {
      
      #f1<-object_eval(result, cursolution)
      
      if (f1<=best_solution_obj){
        best_solution_obj<-f1
        best_solution <- cursolution
      }
      
      newsolution <- cursolution
      
      rand_pos1<-round(runif(1,min=0.5,max=length(cursolution)+0.5))
      rand_pos2<-round(runif(1,min=0.5,max=length(cursolution)+0.5))
      
      tmp <- newsolution[rand_pos1]
      newsolution[rand_pos1] <-newsolution[rand_pos2]
      newsolution[rand_pos2] <- tmp
      
      # rp1_p <- circular_before(rand_pos1, nCities)
      # rp2_p <- circular_before(rand_pos2, nCities)
      # 
      # rp1_n <- circular_after(rand_pos1, nCities)
      # rp2_n <- circular_after(rand_pos2, nCities)
      # 
      # 
      # f2=f1-data_matrix[cursolution[(rp1_p)],cursolution[(rand_pos1)]]-
      #   data_matrix[cursolution[rand_pos1],cursolution[(rp1_n)]]-
      #   data_matrix[cursolution[(rp2_p)],cursolution[(rand_pos2)]]-
      #   data_matrix[cursolution[(rand_pos2)],cursolution[rp2_n]]+
      #   data_matrix[newsolution[(rp2_p)],newsolution[(rand_pos2)]]+
      #   data_matrix[newsolution[(rand_pos2)],newsolution[rp2_n]]+
      #   data_matrix[newsolution[(rp1_p)],newsolution[(rand_pos1)]]+
      #   data_matrix[newsolution[(rand_pos1)],newsolution[rp1_n]]
      
      f2<-object_eval(result, newsolution)
      dE = f2-f1
      if ( dE <0 )  {
        cursolution <- newsolution
        f1 <- f2;
      }
      else {
        if ( exp((-1)*dE/temperature ) > runif(1,0,1) ){
          cursolution <- newsolution
          f1<-f2;
        }
      } 
      index=index+1
      
      # cursolution_list <- rbind(cursolution_list,cbind(index,f1,f2,best_solution_obj,
      # temperature)) 
    }
    
    
    temperature = coolingRate * temperature
    # print(best_solution_obj)
    # print(best_solution)
  }
  
  
  #Write the Solution Process in the data directory and put this into the global solution list. 
  # write.table(cursolution_list, file = "cursolution_list.csv", col.names=TRUE, 
  #             quote=FALSE, row.names=FALSE, sep = ",")
  # 
  #Return the Best Solution Found
  return(best_solution)
}

system.time(best_solution <- SA_V1())
plot_TSP(data,best_solution);
best_solution
object_eval(data_matrix, best_solution)

#system.time(best_solution <- SA_V1())
#f2 eval
#user  system elapsed 
#54.435   0.092  54.648
#user  system elapsed 
#46.099   0.332  46.735 
#
py_sol <- c(0 , 9 , 8 , 5 , 6 , 2 , 10 , 16 , 14 , 13 , 12 , 11 , 15 , 3 , 4 , 1 , 7 , 0)+1
py_sol <- c(0,  13 , 6 , 5 , 17 , 16 , 14 , 15 , 2 , 21 , 22 , 23 , 4 , 25 , 24 , 
            12 , 3 , 9 , 20 , 1 , 10 , 19 , 11 , 8 , 8 , 18 , 0) + 1
py_sol <- c( 0 , 82 , 64 , 55 , 32 , 53 , 15 , 9 , 23 , 36 , 38 , 92 , 1 , 3 , 7 , 33 , 56 , 6 , 43 , 10 , 84 , 76 , 20 , 61 , 47 , 50 , 44 , 81 , 8 , 25 , 78 , 57 , 48 , 46 , 40 , 77 , 52 , 60 , 42 , 34 , 22 , 2 , 19 , 88 , 89 , 16 , 54 , 11 , 5 , 49 , 75 , 18 , 24 , 79 , 29 , 69 , 87 , 35 , 98 , 17 , 59 , 70 , 39 , 95 , 74 , 58 , 83 , 21 , 86 , 14 , 28 , 30 , 96 , 72 , 71 , 37 , 4 , 99 , 65 , 26 , 73 , 67 , 94 , 85 , 45 , 41 , 68 , 93 , 90 , 66 , 27 , 62 , 51 , 91 , 63 , 13 , 31 , 97 , 12 , 80 , 0)+1
object_eval(data_matrix, py_sol)

# 
# py_sol<-c(0 , 13 , 6 , 5 , 17 , 16 , 14  , 15 , 2 , 21 , 22 , 23 , 4 , 25 , 24 , 12 ,
#           3 , 9 , 20 , 1 , 10 , 19 , 11 , 7 , 8 , 18, 0)+1
plot_TSP(data,py_sol);


plot_TSP(data,best_solution);
# 
# table<-read.csv("cursolution_list.csv",head=T,sep=",")
# 
# plot(table$index,table$f1,ylab="evaluation",type="l",col="red")
# par(new=T)
# plot(table$index,table$f2,ylab="evaluation",type="l",col="black")
# par(new=T)
# plot(table$index,table$best_solution_obj,ylab="evaluation",type="l",col="green",
#      ylim=c(min(table$f1),max(table$f1)))
# par(new=T)
# 
# plot(table$index,table$temperature,axes=FALSE,ylim=c(0,500),ylab="",type="l",
#      col="blue")
# axis(4)
# mtext("temperature",side=4,line=3)
# legend("topright",c("f1","f2","best_solution","temperature"),
#        col=c("red","black","green","blue"),cex=0.5,lty=c(1,1,1,1),lwd=c(1,1,1,1),horiz=TRUE)
