lambda<-(sqrt(5)-1)/2
precision<- 0.01

golden.section<-function(f, pL, pU, p1, p2, top, result){
  if (top==26){
    return(result)
  }
  else if(top==1){
    p1<-pL + (1-lambda)*(pU - pL)
    p2<-pU - (1-lambda)*(pU - pL)
  } 
  result[top,]<-c(p1,p2)
  if(f(p2) < f(p1)){
    pU<-p2
    pL<-pL
    p2<-p1
    p1<-pL + (1-lambda)*(pU - pL)
  } else if (f(p2) > f(p1)){
    pU <- pU
    pL <- p1
    p1 <- p2
    p2<-pU - (1-lambda)*(pU - pL)
  }
  result<-golden.section(f, pL, pU, p1, p2, top=top+1, result)
  return(result)
}

result<-data.frame(p1=rep(NA, 25), p2=rep(NA, 25))
func1 <- function(x) (x - 1.235)^2 + 0.78*x + 0.2
func2 <- function(x) 3*x*exp(0.1*x)*sin(x*exp((-0.5)*x))

result<-golden.section(func4,-5, 5, NA, NA, 1, result)

x<-seq(-5,5,by=0.05)
plot(x, func2(x), type="l")
segments(result$p1, seq(-10,-1,length.out=25),
         result$p2, seq(-10,-1,length.out=25))
text(3, seq(-10,-1,length.out=25), labels=1:25, cex=0.8)