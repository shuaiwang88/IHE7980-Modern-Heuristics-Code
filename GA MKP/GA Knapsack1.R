require(stats)
##THe data has objective, coefficient, and rhs
system.time( {

dataset<-read.csv("MKP1-1.csv",header=F)
N<-ncol(dataset)

NR <- 5 
rhs <-seq(1:NR)
for(i in 1:NR) rhs[i] <- dataset[7,i]

lhs <-seq(1:NR)
for(i in 1:NR) lhs[i] <- 0;

#GENERATE 0 MATRIX 
population<-matrix(0,ncol=N,nrow=N)
population=as.data.frame(population)

#GENERATE INITIAL SOLUTION 
for(p in 1:N) {
  
  x<- rep(1:N)
  temp <- sample(x)
  
  for(i in 1:5) lhs[i] <- 0;
  for( k in 1:N ) {   
    item <- temp[k]
    population[p,item] <- 1
## all cells in population are 1
    success <- 1

    for(i in 1:5) { 
      lhs[i] <- lhs[i]+dataset[i+1,item]
      if(lhs[i] > rhs[i]) {success <- 0; break;}
    }

    if(success == 0){
      population[p, item] <- 0; 
      k<- N;  
      break;
    }
  }
}

##########################################################################################
#
#  This is the mainbody of your GA
#
##########################################################################################

step<-0
fitness<-vector(mode="numeric")
fitness<-as.data.frame(fitness)
for (j in 1:N){
  fitness[1,j]<-sum(dataset[1,]*population[j,])
}

max_index<-which(fitness==max(fitness))
max<-fitness[1,max_index]


best_objective<-data.frame()

#THis is the Child Generate by Two parents
child<-NULL
child<-as.data.frame(child)

resource<-vector(mode="numeric")
resource<-NULL

for (step in 1:100){
  ## Random Select Two Parents, Do a Tournameent, as Parent 1
  rand_1<-sample(1:100,2)
  pool1<-population[rand_1,]
  if (fitness[rand_1[1]]>fitness[rand_1[2]] ){
    parent1<-population[rand_1[1],]
  }else{
    parent1<-population[rand_1[2],]
  }
  
  ## Random Select Two Parents, Do a Tournameent, as Parent 2
  rand_2<-sample(1:100,2)
  pool2<-population[rand_2,]
  if (fitness[rand_2[1]]>fitness[rand_2[2]] ){
    parent2<-population[rand_2[1],]
  }else{
    parent2<-population[rand_2[2],]
  }
  
  #### Cross Over -- Uniform
   for (i in 1:N){
    rand_3<-runif(1,0,1)
    if (rand_3>0.5){
      child[1,i]<-parent1[i]
    }else{
      child[1,i]<-parent2[i]
    }
  }

  ## Mutuation 
  Pm=0.02
  for (i in 1:N){
    rand_4<-runif(1,0,1)  
    if (rand_4<Pm){
      if (child[1,i]==1){
        tmp<-0
        child[1,i]<-tmp
      }else{
        tmp_1<-1
        child[1,i]<-tmp_1
      }
    }
  }

  ## Repair Operator 
  for (i in 1:5){
    resource[i]<-sum(dataset[i+1,]*child)
    #resource[i]<-sum(dataset[i+1,]*child[child>0])
  }
  
  ###DROP
    for (i in N:1){
    infeasible <- 0;
    
    if (child[i]==1) {
      for (j in 1:5){
        if(resource[j]>rhs[j]) {infeasible <- 1; break;}
      }
      if(infeasible ==1 ) { 
        child[i]<-0
        for (j in 1:5){
          resource[j]=resource[j]-dataset[j+1,i]
        }
      }
    }
  }
  
  ###ADD
  for (i in 1:N){
    if(child[i]==0){
      feasible <- 1;
      for (j in 1:5){
        if (resource[j]+dataset[j+1,i]>rhs[j]){
          feasible <- 0; break;
        }
      }
      if(feasible==1) {
        child[i] <- 1
        for(j in 1:5) {
          resource[j]<-resource[j]+dataset[j+1,i]
        }
       }
    }
  }
  
  ## Discard any duplicate children
  identification<-NULL
  identification<-as.logical()
  same <- 0
  for (i in 1:N){
    identification[i]<-identical(child,population[i,])
#   if(identification[i]==TRUE) {same<-1; break;}
  }
  
  if (any(identification==TRUE)==FALSE){
#  if(same==0) {
    objective_child<-sum(dataset[1,]*child)
    
    min_index_list<-which(fitness==min(fitness))
    min_index <- min_index_list[1];
    
    population[min_index,]<-child 
    # objective<-sum(dataset[1,]*population[min_index,]) 
    fitness[1,min_index]<-objective_child
    
    if(objective_child>max){
      max<-objective_child
      fitness [1,max_index]<-child
    }
    best_objective<-rbind(best_objective,cbind(step,max))
    print(list(step,max))
  }
  
  write.table(best_objective, file = "max.txt", col.names=TRUE, quote=FALSE, row.names=FALSE, sep = ",")
}

#Rprof(NULL)
#summaryRprof()

table<-read.table("max.txt",header=T,sep=",")
plot(table,type="b",col="red")



##Validation 
max_index<-which(fitness==max(fitness))
max<-fitness[1,max_index[1]]
sum(population[max_index[1],]*dataset[1,])
resource_test<-NULL
for (i in 1:5){
  #resource[i]<-sum(dataset[i+1,] *population[max_index,])
  resource_test[i]<-sum(population[max_index[1],]*dataset[i+1,] )
  
}
resource_test
print(rhs)
best_child<-population[max_index[1],]
best_child
})