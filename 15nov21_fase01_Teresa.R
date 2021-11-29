#creando funcion con 1 parametro de entrada
estadistica<- function(x){
  z<- ceiling(length(x)/2)  #es el centro de la tabla
  tabla<- data.frame(x)
  tabla[z,"Media"]<-mean(x) #quiero que me imprima la media
  tabla[z,"DE"]<-sd(x)  #desviacion estantar
  tabla
}
estadistica(a)

propagacion<-function(x,y){
  
  centrado<-ceiling(length(y)/2)
  tabla<-data.frame(x,y)
  tabla [,"g"]<-4*pi^2*x/y^2   # calculo los g x=l y=T
  tabla[centrado,"gprom"]<-mean(tabla$g) #gpromedio4
  tabla[centrado,"sigmag"]<-sd(tabla$g)/sqrt(length(x)) 
  print(tabla) #imprime la tabla en r
  library(xtable)
  xtable(tabla)
}

l<-c(0.512,0.597,0.682,0.797,0.883)
T<-c(1.448,1.566,1.669,1.804,1896)
propagacion(l,T)