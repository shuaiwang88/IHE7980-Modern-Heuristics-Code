#########################################################
# The following function is adopted from page 104, R book 
#     to read the matrix
ptm <- proc.time()
line.number<-length(scan("scp410.txt",sep="\n"))
my.list<-sapply(0:(line.number-1),
                function(x) scan("scp410.txt",skip=x,nline=3,quiet=T))
#########################################################
#Rprof()
system.time()
NumPop <- 100;
NumRow<-my.list[[1]][1]  #200
NumCol<-my.list[[1]][2]  #1000

obj_coef <- my.list[[2]]
obj_coef<-as.vector(obj_coef)

row_table<-my.list[3:202]
row_table_length <- rep(0, NumRow)

for (i in 1:NumRow){
  row_table[[i]]<-row_table[[i]][-1] # remove the row length number
  row_table_length[i]<-length(row_table[[i]])
}

col_table<-rep( list(vector()), NumCol ) 
col_table_length <- rep(0, NumCol)

for(i in 1:NumRow) {
  row<-row_table[[i]]
  for(j in 1:row_table_length[i]) {
    index <- row[j];
    col_table_length[index] = col_table_length[index] +1
    number_of_ele = col_table_length[index];
    col_table[[index]][number_of_ele] = i
  }
} 

pop_table<-rep( list(vector()), NumPop ) 
pop_obj_table <- rep(0, NumPop )

for(p in 1:NumPop) {
  print(p)
  
  aSolution<-NULL
  aSolution<-vector(mod="numeric")
  
  Wi<-NULL # inlitialize 
  Wi<-mat.or.vec(NumRow, 1) # intilize Wi=0   200 0s mat.or.vec
  
  index <- 1
  for (i in 1:NumRow){
    col_index<-sample(1:row_table_length[i],1)
    col<-row_table[[i]][col_index]  #RANDOM GET A COL FROM ROW_TABLE
    
    #Check if the col is in the solutions or not
    #If yes, do not include it 
    #Otherwise, update Wi
    if(!is.element(col,aSolution)){
      aSolution[index]<-col
      for(j in 1:col_table_length[col]){
        item_row <- col_table[[col]][j]
        Wi[item_row] <- Wi[item_row] +1;
      }
      index = index + 1;
    }
  }
  
  tmp_solution<-sample(aSolution,length(aSolution))
  
  for(k in 1:length(tmp_solution)) {
    rand_col2 <- tmp_solution[k] 
    
    #Reduce The Columns and Check for Wi
    for(i in 1:col_table_length[rand_col2]){
      row_index <- col_table[[rand_col2]][i]
      Wi[row_index] <- Wi[row_index] - 1;
    }
    
    #CHeck if the solution is feasible or not
    infeasible = 0;
    for(i in 1:NumRow){
      if(Wi[i]==0 ) { infeasible = 1; break;}
    }
    
    #If feasible, get rid of the colmn from solution
    if(infeasible == 0) {
      aSolution=aSolution[which(aSolution!=rand_col2)]
    }
    #Else, update Wi to its original varlue
    else {
      for(i in 1:col_table_length[rand_col2]){
        row_index <- col_table[[rand_col2]][i]
        Wi[row_index] <- Wi[row_index] + 1;
      }
    }
  }

  pop_table[[p]] = aSolution;
  pop_obj_table[p] = sum(obj_coef[aSolution])
}

fitness_pop<-NULL
fitness_pop<-vector(mode="numeric")

for (j in 1:NumPop){
  fitness_pop[j]<-sum(obj_coef[pop_table[[j]]])
}

min_pop_index<-which(fitness_pop==min(fitness_pop))
min_pop_fitness<-fitness_pop[min_pop_index]

best_objective<-NULL

for (step in 1:3000){
  print(step)

  ## Random Select Two Parents, Do a Tournameent, as Parent 1
  rand_1<-sample(1:NumPop,2)
  if (pop_obj_table[rand_1[1]]>pop_obj_table[rand_1[2]]){
    parent1<-pop_table[rand_1[1]]
  }else{
    parent1<-pop_table[rand_1[2]]
  }
  parent1<-unlist(parent1)
  fitness_p1<-sum(obj_coef[parent1])

  ## Random Select Two Parents, Do a Tournameent, as Parent 2
  rand_2<-sample(1:NumPop,2)
  if (pop_obj_table[rand_2[1]]>pop_obj_table[rand_2[2]]){
    parent2<-pop_table[rand_2[1]]
  }else{
    parent2<-pop_table[rand_2[2]]
  }
  parent2<-unlist(parent2)
  fitness_p2<-sum(obj_coef[parent2])

  ##### Cross Over------fusion opeartor crossover
  aChild_p1<-NULL
  aChild_p1<-vector(mod="numeric")

  aChild_p2<-NULL
  aChild_p2<-vector(mod="numeric")
  index <- 0
 
  SameCols <- intersect(parent1, parent2)
  DiffCols_p1 <- setdiff(parent1, parent2)
  DiffCols_p2 <- setdiff(parent2, parent1)

  AccpProb <- fitness_p1/(fitness_p1+fitness_p2);
  for (i in 1:length(DiffCols_p1)){
    probability1<-runif(1,0,1)
    if(probability1>AccpProb) {
      aChild_p1[i]<-DiffCols_p1[i]  
    }
  }
  aChild_p1<-aChild_p1[!is.na(aChild_p1)]

  for (i in 1:length(DiffCols_p2)){
    probability1<-runif(1,0,1)
    if(probability1<AccpProb) {
      aChild_p2[i]<-DiffCols_p2[i]  
    }
  }
  aChild_p2<-aChild_p2[!is.na(aChild_p2)]
  
  aChild<-c(SameCols,aChild_p1,aChild_p2)  

  #########Mutation##########
  Pm=0.003 # probablity of mutation
  nBitMutation <- rbinom(1,NumCol, Pm)
  for (i in 1:nBitMutation){
    rand_col<-sample(1:NumCol,1)
    if(is.element(rand_col,aChild)==FALSE){
      nl <- length(aChild)
      aChild[nl+1] <- rand_col
    }
    else{
      aChild <- aChild[which(aChild!=rand_col)]
    } 
  }

  #########Repair Operation ##########
  #SetS : the set of columns in a solution
  #SetU : the set of uncovered rows
  #Initialise  U  := {i| Wi  = O, Vi E I}.
  #Wi =|S intercsect ai| i E I
  
  SetS<-aChild
  Wi1<-NULL # inlitialize 
  Wi1<-mat.or.vec(NumRow, 1) # intilize Wi=0   200 0s mat.or.vec
  
  for(i in 1:NumRow){
    row <- row_table[[i]]
    Wi1[i] <-length(intersect(row,SetS))
  }  
  
  
  SetU<-which(Wi1==0)
  while(length(SetU)>0 ){
    which_row = SetU[1]
    col_cover_row = row_table[[which_row]]
  
    min_val <- 1000;
    min_index <- NULL;
    for(j in 1:length(col_cover_row)) {  
      col <- col_cover_row[j];
      val <- obj_coef[col]/length(intersect(SetU,col_table[[col]]))
      if(val <= min_val) {
        min_val <- val
        min_index <- col_cover_row[j]
      }
    }      
    aChild<-c(min_index,aChild)
    SetU = setdiff(SetU, col_table[[min_index]]) 
  }

  #SetU<-which(Wi1==0)
  SetS<-aChild
 
  
  Wi2<-NULL # inlitialize 
  Wi2<-mat.or.vec(NumRow, 1) # intilize Wi=0   200 0s mat.or.vec
  
  for( i in 1:NumRow){
    row <- row_table[[i]]
    Wi2[i] <-length(intersect(row,SetS))
  }
  
  ########################################
 
  #drop Wi
  ### S  = the set  of columns in  a  solution
  #SetS<-aChild
  
  for(k in length(aChild):1){
    col<-aChild[k]
    
    for(i in 1:col_table_length[col]){
      row_index <- col_table[[col]][i]
      Wi2[row_index] <- Wi2[row_index] - 1;
    }
    infeasible = 0;
    for(i in 1:NumRow){
      if(Wi2[i]==0 ) { infeasible = 1; break;}
    }

    #If feasible, get rid of the colmn from solution
    if(infeasible == 0) {
      aChild=aChild[which(aChild!=col)]
    }
    #Else, update Wi to its original varlue
    else {
      for(i in 1:col_table_length[col]){
        row_index <- col_table[[col]][i]
        Wi2[row_index] <- Wi2[row_index] + 1;
      }
    }
  }
  
  ###################################
  ##to discard any duplicate children and update
  identification<-NULL
  identification<-numeric()
 
  #ptm <- proc.time()
  for (g in 1:NumPop){
    identification[g]<-length(setdiff(aChild,pop_table[[g]]))
  }
  #proc.time() - ptm                   # 0.01s 

 if (any(identification==0)==0){
    fitness_child<-sum(obj_coef[aChild])
    
    max_pop_index_list<-which(fitness_pop==max(fitness_pop))
    max_pop_index <- max_pop_index_list[1];
    
    pop_table[[max_pop_index]]<-aChild
    fitness_pop[max_pop_index]<-fitness_child
      
    if(fitness_child<min_pop_fitness){
      min_pop_fitness<-fitness_child
      pop_table[[min_pop_index]]<-aChild
    }
    print(min_pop_fitness)
   # print(mean(identification))
    best_objective<-rbind(best_objective,cbind(step,min_pop_fitness,
                                               LENGTH=mean(identification)))
  }
  
  write.table(best_objective, file = "max.txt", col.names=TRUE,
              quote=FALSE, row.names=FALSE, sep = ",")
}
proc.time() - ptm
plot(best_objective)

