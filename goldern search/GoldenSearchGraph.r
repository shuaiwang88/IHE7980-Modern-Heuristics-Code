### This is the code used to perform a golden search
### the return is the search process, 
golden_search<-function(f, a, b, tol = 1e-9){
  gtable=NULL
  lamda=0.5*(sqrt(5)-1)
  while (b-a>tol){
    x1<-a+(1-lamda)*(b-a)
    x2<-b-(1-lamda)*(b-a)
    tmp<-cbind(a, f(a), x1, f(x1), x2,f(x2), b, f(b))
    gtable<-rbind(gtable, tmp)

    if(f(x1)>=f(x2)){
      b<-x2; a=a; x2<-x1; x1<-a+(1-lamda)*(b-a)
    }else{
      b<-b;  a<-x1; x1<-x2; x2<-b-lamda*(b-a)
    }
  }
  colnames(gtable) <- c("a", "f(a)", "x1", "f(x1)", 
                        "x2","f(x2)"," b", "f(b)")
  return (gtable)
}


## This is a test example. 

#f0<-function(x) (3*x + 13 /x + 1.7)*(-1)
#f1<-function(x) exp(sin(exp(x)))
#f2<-function(x) 3*x*exp(0.1*x)*sin(x*exp((-0.5)*x))

f0<-function(x) (4.1*x^2-5*x-6.5)

result<-golden_search(f0,0,1,1e-9)
write.table(result, file = "golden search_unimodal.csv", 
            col.names=TRUE, quote=FALSE, 
            row.names=FALSE, sep = ",")

x<-seq(from=0,to=1,by=1e-9)
y<- f0(x);
plot(x,y,type="l")

min_y <- min(y); max_y <- max(y);
index<-which.min(y);index1<-which.max(y)
min_x <- x[index]
max_x <- x[index1]

segments(result[,1],seq(min_y,nrow(result)),
         result[,7],seq(min_y,nrow(result)), col="red")
text(3,seq(min_y,nrow(result)),
     labels=1:nrow(result),cex=0.6, col="red")


result<-golden_search(f2,-4,4,1e-9)
write.table(result, file = "golden search_nounimodal.csv", 
            col.names=TRUE, quote=FALSE, 
            row.names=FALSE, sep = ",")

x<-seq(from=-4,to=4,by=0.01)
y<- f2(x);
plot(x,y,type="l")

min_y <- min(y); max_y <- max(y);
index<-which.min(y);index1<-which.max(y)
min_x <- x[index]
max_x <- x[index1]
segments(result[,1],seq(min_y,nrow(result)),
         result[,7],seq(min_y,nrow(result)), col="red")
text(3,seq(min_y,nrow(result)),
     labels=1:nrow(result),cex=0.6, col="red")




