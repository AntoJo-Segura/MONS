#noise modelling
#Kernel develpment

#The same code with R language
#` Data
set.seed(1)
data = c(rnorm(100,-10,1),rnorm(100,10,1))
#` True
phi = function(x) exp(-.5*x^2)/sqrt(2*pi)
tpdf = function(x) phi(x+10)/2+phi(x-10)/2
#` Kernel
h = sd(data)*(4/3/length(data))^(1/5)
Kernel2 = function(x) mean(phi((x-data)/h)/h)
kpdf = function(x) sapply(x,Kernel2)
#` Plot
x=seq(-25,25,length=1000)
plot(x,tpdf(x),type="l",ylim=c(0,0.23),col="red")
par(new=T)
plot(x,kpdf(x),type="l",ylim=c(0,0.23),xlab="",ylab="",axes=F)