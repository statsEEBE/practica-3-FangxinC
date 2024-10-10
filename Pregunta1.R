#ensayo de bernoulli
x <- c(0,1)
f <- c(0.68,0.32)
plot(x,f,type="h",ylim=c(0,1),col="red")
points(x,f,pch=16,col="red")

n = 43 #numero de encuesta
muestra <- sample(x,n,f, replace=TRUE)
pie(table(muestra))#grafica de sectores
mean(muestra)#media
table(muestra)/n #frequencia relativa
bar<-barplot(table(muestra)/n, ylim=c(0,1))
lines(bar,f,type="h",ylim=c(0,1),col="red")
points(bar,f,pch=16,col="red")

Y <- function(i){sum(sample(x,n,f, replace=TRUE))}
set.seed(122)
m<-40000
barplot(table(sapply(1:m,Y)))
encuesta <- sapply(1:m,Y)
fr <- table(encuesta)/m
fr["13"]

dbinom(13,43,0.32)
xx <- names(fr)
xx
br <- barplot(table(encuesta)/m)

dbinom(17,44,0.32)
plot(0:43,dbinom(0:43,44,0.32),type='h',col='red')#funcion de frequencia
pbinom(16,44,0.32)

n<- 24
x<- c(0,1)
f<- c(0.32,0.68)
xstar <- function(i){sum(sample(x,n,f, replace=TRUE))}
set.seed(122)
m<-4000
encuestas <- sapply(1:m, xstar)
mean(encuestas)
var(encuestas)
