
dataset<-read.table("knapsack.txt")
lchrom<-50
posize<-80
Pc<-0.7
Pm<-0.1
maxgen<-5000
#alpha<=     ## penality parameter

###############################################get the weight and profit from data file  
profit<-dataset[c(1:lchrom),]
profit<-as.numeric(profit)

weight<-dataset[c((lchrom+1):(2*lchrom)),]
weight<-as.numeric(weight)

weightlimit <-dataset[2*lchrom+1,]###  the last value in the dataset 
weightlimit<-as.numeric(weightlimit)

##############################generate chromosome 

chromosome<-function(chromosome){
  chromosome<-sample(c(0,1),lchrom,replace=T)
  return(chromosome)
}

#####################generate initial population considering the weight limt constraint

population<-function(population){
  population=NULL
  for (i in 1:posize){
    population<-rbind(chromosome(),population)  
  }
  
  for (i in 1:nrow(population)){
    
    cur_weight<-sum(weight*population[i,])
    
    while(cur_weight>weightlimit){
      tmp<-sample(c(0,1),lchrom,replace=T)
      cur_weight<-sum(weight*tmp)
      if (cur_weight<weightlimit){
        population[i,]<-tmp
        
      }
    }
   # print(cur_weight)
  }
  
  return (population)
}
population<-population()
population=as.data.frame(population)

####################calculate the  weight and profit#############################################

cur_weight_eval<- function(x) { 
 for (i in 1:posize)
   {
   cur_weight<-sum(weight*population[i,]) 
   #print(cur_weight)
 }
 return(cur_weight)
 }  


cur_profit_table<-NULL
cur_profit_eval<- function(x) {
  index=0
  for (i in 1:posize){
  cur_profit<-sum(profit*population[i,])
  index=index+1
 # print(cur_profit)
  tmp<-cbind(index,cur_profit,population[i,])
  
  cur_profit_table<<-rbind(cur_profit_table,tmp)
  }
  #return(cur_profit_table)
}
cur_profit_eval()

#####get the total , max and min of profit #########
###total
sumprofit<-sum(cur_profit_table$cur_profit)
avergprofit<-sumprofit/posize

minprofit<-min(cur_profit_table$cur_profit)
minprofit_chrom<-cur_profit_table[which(cur_profit_table$cur_profit==min(cur_profit_table$cur_profit)),2:51]

maxprofit<-max(cur_profit_table$cur_profit)
maxprofit_chrom<-cur_profit_table[which(cur_profit_table$cur_profit==max(cur_profit_table$cur_profit)),2:51]

########################
#sort the populoation
cur_profit_table=cur_profit_table[order(cur_profit_table$cur_profit, decreasing = T),]
####get the fitness of profit of each chrom
cur_profit_table$profit_fitness<-cur_profit_table$cur_profit/sumprofit

cur_profit_table<-subset(cur_profit_table,select=c(index,cur_profit,profit_fitness,V1:V50)) ## move the prfit_fitness to the second coloumn 

# get the acculmulated profitness of profit
sum_profit_fitness<-sum(cur_profit_table$profit_fitness)


####





#choose the two parents 
P1<-cur_profit_table[1,4:53]
P2<-cur_profit_table[2,4:53]
#print(P1)
#print(P2)
##cross over
#point_crossover<-as.numeric(sample(1:lchrom-1,1,replace=T)) 
 
#tmp<-P1[point_crossover:lchrom]
#P1[point_crossover:lchrom]<-P2[point_crossover:lchrom]
#P2[point_crossover:lchrom]<-tmp
 
### mutation
###
#mutation=function(){
  #if(Pm>runif(1)){
  #  point_mutate<-as.numeric(sample(1:lchrom,1))
  #  new_population_table<-cur_profit_table[-c(1:3)]
  #  sample(0:1,1)
  #}
  
#}













