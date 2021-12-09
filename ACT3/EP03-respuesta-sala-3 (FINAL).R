#Script sala 3
library(ggpubr)
dir <- "c:\\Users\\ariel\\Downloads"   #cambiar dependiendo de la dirección de su archivo
basename <- "Casen 2017.csv"
file <- file.path(dir, basename)
población <- read.csv(file = file)
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(185)#nueva semilla
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
#grafico distribución normal
y <- dnorm(ingreso.normal, mean = media.ingreso, sd = sd.ingreso)
normal <-data.frame(ingreso.normal,y)
g1 <- ggplot(normal,aes(ingreso.normal,y))+geom_line(color="blue")
g1

#distribución Z mediante la ecuación
distribucionZ <- (ingreso.normal - media.ingreso)/sd.ingreso
#gráfico para distribución Z
hist(distribucionZ, prob= TRUE, main = "Distribucion Z", ylab = "Densidad",xlab = "Ingresos Totales")
x <- seq(min(distribucionZ), max(distribucionZ), length = 40)
y <- dnorm(x, mean = mean(distribucionZ), sd = sd(distribucionZ))
lines(x, y, col = "red", lwd = 2)

#distribución con 6 grados de libertad
distribucion1<-c()
i<-0
while(i<5000){
  x<-sample(distribucionZ,6, replace = F)
  x<-x^2
  distribucion1<-c(distribucion1, sum(x))
  i<-i+1
}
#gráfico para distribución con 6 grados de libertidad
hist(distribucion1, prob= TRUE, main = "Distribución con 6 grados de libertad", ylab = "Densidad",xlab = "Ingresos Totales")

#distribución con 10 grados de libertad
distribucion2<-c()
i<-0
while(i<5000){
  x<-sample(distribucionZ,10, replace = F)
  x<-x^2
  distribucion2<-c(distribucion2, sum(x))
  i<-i+1
}
#gráfico con distribución con 10 grados de libertad
hist(distribucion2, prob= TRUE, main = "DistribuciÓn con 10 grados de libertad", ylab = "Densidad",xlab = "Ingresos Totales")

#distribución F respecto a la distribución 1 y 2
distribucionF<- (distribucion1/6)/(distribucion2/10)
#gráficO con distribución F
hist(distribucionF, prob= TRUE, main = "Distribución F", ylab = "Densidad",xlab = "Ingresos Totales")


