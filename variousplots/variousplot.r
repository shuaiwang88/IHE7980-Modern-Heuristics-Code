#1.1
x1<-seq(5.5,7.15,by=0.001)
y1<-exp(sin(exp(x1)))
plot(x1,y1, type='l')

#1.2
x2<-seq(-4,2,by=0.001)
y2<-x2*sin(1/(x2+2))
plot(x2,y2,)

#1.3
library(scatterplot3d)
attach(mtcars)
x3<-seq(from=-4,to=4,by=0.01)
y3<-seq(from=-4,to=4,by=0.01)
z=0.5*exp((-0.01)*(x3^2+y3^2))-sin(sqrt(x3^2+y3*2))^2/(1+0.001*(x3^2+y3^2))
scatterplot3d(x3,y3,z)

library(akima)
func3 <- function(x3,y3) 0.5*exp((-0.01)*(x3^2+y3^2))-sin(sqrt(x3^2+y3*2))^2/(1+0.001*(x3^2+y3^2))  
contour(x3,y3,outer(x3,y3,func3), add=T)

z3<-func3(x3,y3)

func4 <- function(x,y) 3*x*exp(0.1*x)*sin(y*exp((-0.5)*x))
z4<- func4(x3,y3)
scatterplot3d(x3,y3,z4)

contour(x3,y3,outer(x3,y3,func4), add=T)

#Draw a volcano
library(lattice)
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

#1.4
y4=seq(-5,5,by=0.01)
x4=Inf
x4^2*y4^2=(y4+3)^2*(25-y4^2)