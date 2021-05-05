# TEOREMA CENTRAL DEL LIMITE
#definimos el tamaño de la muestra
n=10000

#definimos el número de veces que repetimos el muestreo
nm=2000


# preparamos una matriz para guardar las muestras que vayamos generando con tantas columnas como tamaño de la muestra y filas por el número de muestras que hayamos definido
# y además generamos un vector para almacenar la suma de las muestras independientes que hayamos definido (nf) que sería el concepto del TCL
mt=matrix(1:nm*n, nm,  n)
TCL=vector()
mediax=vector()

#Vamos a definir una función que nos va a generar muestras aleatorias en función de las distribuciones que elijamos
#  "unif" función uniforme entre 10 y 20
#  "binom" función binomial con con p=0,3
#  "chisq" función chicuadrado con 10 grados de libertad
#  "exp" función exponencial con lambda=20





#dist="unif" 
#dist="binom" 
dist="chisq" 
#dist="exp" 


genera=function(dist,n){
  if (dist=="unif"){
    return(runif(n,10,20))    
  } else if (dist=="binom"){
    return(rbinom(n,n,0.3))
  } else if (dist=="chisq"){
    return(rchisq(n,20,ncp=0))  
  } else {
    return(rexp(n,20))
  } 
}


# Generamos nm muestras aleatorias y las guardamos en la tabla que definimos antes
# y además almacenamos el valor de la suma de funciones en TCL

for (i in 1:nm){
  x=genera(dist,n)
  mt[i,]=x
  mediax[i]=mean(x)
}
TCL=colSums(mt) 

mediademediax=mean(mediax)
mediademediax

# representamos dos histogramas de las muestras generadas y el del la suma de muestras (TCL) 
par(mfrow=c(3, 2))

hist(mt[nm,],probability=TRUE)
hist(mt[nm-1,],probability=TRUE)
hist(TCL,probability=TRUE)
curve(dnorm(x, mean(TCL), sd(TCL), log = FALSE),col='red', add = TRUE)
hist(mediax,probability=TRUE)
curve(dnorm(x, mean(mediax), sd(mediax), log = FALSE),col='red', add = TRUE)


plot(1:length(mediax), cumsum(mediax)/(1:length(mediax)),lwd=2, type="l", ylab="Media muestral", xlab="Nº de repeticiones")

