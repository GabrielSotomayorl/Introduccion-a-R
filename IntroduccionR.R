#================================
#    Introducción al uso de R
#================================
install.packages("haven")
install.packages("car")
install.packages("dplyr")
install.packages("moments")
install.packages("PropCIs")
install.packages("Publish")
install.packages("corrplot")
#Objetos de diversos tipos------------------------------------------------------

#Creemos el primer objeto, con el operados <- asignamos los objetos
x<-5
#Luego podemos imprimir el objeto que creamos
x
#creemos un segundo objetode nombre y con el valor 10
y<-10
#Ahora podemos realizar operaciones con nuestros objetos
x+y
x-y
x*y
x/y

z<-x^2 #podemos guaradar los resultados como objetos
z
#También podemos realizar opreaciones lógicas
x>y 
15==x+y #Para restablecer una igualdad usamos doble signo =
16>x+y

#Creación de una variable. "Edad". mediante la función concatenar "c()",
#podemos crear un objeto que agrupe un conjunto de datos. Para el lenguaje
#del software esto es un vector, para nosotros una variable, en este caso
#numérica (numeric): intervalar, continua, cuantitativa.

Edad<- c(18,25,33,38,67,25,35,57,99)
summary(Edad)
table(Edad)
class(Edad)

#También podemos realiar operaciones sobre los vectores
Edad/2
Edad-1
Edad2<-Edad-1 #y guardar los resultados

#Creación de una variable. "Sexo". Se sigue la misma lógica. Variable 
#cualitativa y nominal, dicotómica.
#Tipo "Character"
#Categorías: H=Hombre; M=Mujer.

Sexo<-c("H","H","H","M","H","M","M","M")

summary(Sexo)
table(Sexo)
class (Sexo)

#También puede expresarse como factor siendo variable dummy (para 1 y 0). 
#Variable cualitativa, nominal.
S<-c(1,1,1,0,1,0,0,0,9,9)

SEXO<-factor(S, levels = c(0,1,9), labels = c("Mujer","Hombre")) #importancia
#                                                    de los errores
SEXO<-factor(S, levels = c(0,1,9), labels = c("Mujer","Hombre","NC"))

summary(SEXO)
table(SEXO)

#Variable Nivel socioeconómico. Ordinal, cualitativa.
# NSE: 1=E, 2=D, 3=C3, 4=C2, 5=C1, 6=AB

P1<-c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,5,5,5,6,99,99)

NSE<-factor(P1,levels=c(1,2,3,4,5,6),labels=c("E","D","C3","C2","C1","AB"))
table(NSE)
summary(NSE) #NA son lo perdidos

#Podemos seleccionar elementos específicos de los vecotres
NSE[4] #pedimos el cuarto elemento
NSE[1:9] #los primeros 9
NSE[c(3,4,5,8,24,31)]
NSE[37]
NSE[c(T,F)]
NSE[NSE=="AB"]
length(NSE)
class(NSE)

#Crear listas
x <- list(u=c(2,3,4), v="abc")
x #el elemento u de la lista es un vector con 3 números, y el elemento v es abc
#ver el elemento u, de la lista x
#Pedir elementos
x$u
x[[2]]
x[[1]][2]

str(x) #este comando muestra la estructura de un objeto de manera resumida

#limpiar el ambiente de trabajo
remove(Edad) #borrar un objeto particular
remove(list = ls()) #Borrar todo

#Trabajo con bases de datos-----------------------------------------------------
#Ahora pasaremos a trabajar con bases de datos de encuestas, para esto primero 
#debemos fijar un directorio de trabajo

setwd("C:/Users/Gabriel/Desktop/Directorio R")
# Nota: Debe usarse el simbolo "/" y NO "\"

#Identificar directorio
getwd()

#Ahora que hemos establecido una conexión entre R y un directorio, debemos abrir
#los datos con que trabajaremos, partiremos con una base de datos de SPSS

#Para esto ocuparemos el primer paquete
library(haven) #Este comando nos permite ejecutar el paquete
cep<- read_spss("Encuesta CEP 83 May 2019 v1") #error por extensión del archivo
cep<- read_spss("Encuesta CEP 83 May 2019 v1.sav")


#para seleccionar una varaible de uan base de datos usamos $
cep$GSE #base$variable
#Tambien podemos usar corchetes base[filas,columnas]
cep[,3]
cep[854,3]


library(dplyr) #Cargar paquete, si no está cargado desde antes.
CEP <- select(cep, sexo = DS_P1, region = REGION, edad = DS_P2_EXACTA,
              satisfaccion_vida = SV_1, satisfaccion_chilenos = SV_2, 
              eval_econ = MB_P2, dif_ing= M2_P4_1,GSE=DS_P44)

remove(cep)

#Se indica base de datos, el nombre de variable a crear y los datos que la compondrán.
View(CEP) #Visualización de la base
head(CEP) #Nos muestra las primeras filas de la base de datos
str(CEP) 
dim(CEP) #dimensiones de la base

#Transformar a una variable distinta, categorías "Otras regiones" y "Región Metropolitana".
CEP <- mutate(CEP, region_factor = car::recode(CEP$region, "1:12 = 1; 13 = 2; 14:16 = 1"))
#Sobreescribir variable con resultado de convertir a factor incorporando etiquetas
CEP$region_factor <- factor(CEP$region_factor, #podemos omitir levels
                            labels = c("Otras regiones", "Región Metropolitana"))
table(CEP$region_factor) 

CEP$GSE <- factor(CEP$GSE, levels = c(1,2,3,4,5),
                            labels = c("ABC1", "C2","C3","D","E"))

#Estadísticos descriptivos------------------------------------------------------
#Trabajaremos con la variable dif_ing
#¿Qué tan de acuerdo o en desacuerdo está Ud. con las siguientes afirmaciones?
#   A.	Las diferencias de ingreso en Chile son demasiado grandes
#1.	Muy de acuerdo
#2.	De acuerdo	
#3.	Ni de acuerdo ni en desacuerdo	
#4.	En desacuerdo	
#5.	Muy en desacuerdo	
#8.	No sabe (no leer)	
#9.	No contesta (no leer)	

mean(CEP$dif_ing) #media
summary(CEP$dif_ing) #los CASOS PERDIDOS!!!
table(CEP$dif_ing)

#Dar casos por perdidos
CEP$dif_ing[CEP$dif_ing==8]<-NA #Aqui vemos un uso de las comparaciones lógicas
CEP$dif_ing[CEP$dif_ing==9]<-NA
summary(CEP$dif_ing) 

#Medidas de tendencia central
mean(CEP$dif_ing,na.rm = TRUE) #agregamos na.rm=TRUE para no considerar los NA
mean(CEP$dif_ing,na.rm = TRUE, trim = 0.025) #mdia recortada
median(CEP$dif_ing,na.rm = TRUE) #mediana

#Medidas de dispreción
min(CEP$dif_ing,na.rm = TRUE) #mínimo
max(CEP$dif_ing,na.rm = TRUE) #máximo
var(CEP$dif_ing,na.rm = TRUE) #varianza
sd(CEP$dif_ing,na.rm = TRUE) #desviación estandar
sd(CEP$dif_ing,na.rm = TRUE)/mean(CEP$dif_ing,na.rm = TRUE) #Coeficiente de varaición

#Medidas de posición
quantile(CEP$dif_ing, probs = c(0.25,0.50,0.75,0.9),na.rm = TRUE) #percetniles
table(CEP$GSE) #tabla de frecuencia
prop.table(table(CEP$GSE)) #frecuencias relativas
tab_porcentaje<-prop.table(table(CEP$GSE))*100 #frecuencias relativas porcentuales
round(tab_porcentaje,digits = 2)

#Medidas de forma
library(moments)
skewness(CEP$dif_ing,na.rm = TRUE) #asimetria
kurtosis(CEP$dif_ing,na.rm = TRUE) #curtosis

#Intervalos de confianza para proporciones
library(PropCIs)
table(CEP$GSE)

nrow(CEP)
# Intervalo de confianza para nivel D
exactci(x = 506, n = 1350, conf.level = 0.95)

#Intervalos de confianza para medias
library(Publish)
ci.mean(CEP$dif_ing) #Nivel de confianza por defecto 0.05.

ci.mean(CEP$dif_ing, alpha = 0.1)

#Los objetos solo existen en la memoria temporal, si queremos guardar un objeto
#debemos usar el comando save
save(CEP, file = "seleccion_CEP.RData")

#visualización básica-----------------------------------------------------------

#Gráfico de barras
plot(CEP$GSE, main="Grupo Socioeconómico", xlab="GSE",ylab="Frequencias",
     col=2)

#Gráfico de torta
valores<-as.numeric(prop.table(table(CEP$region_factor)))
pie(valores, labels = c("Otras regiones 59,2%","Región Metropolitana 40,8%"),
    main = "Gráfico de torta 1",
    sub = "Región")

#Histogramas
hist(CEP$edad,main="Edad de los encuestados", xlab="Años",ylab="Frequencias")

#boxplot
boxplot(CEP$edad, main = "Gráfico de cajas edad",
        outline = TRUE)


#Estadísticos bivariados--------------------------------------------------------
#Prueba T
#Primero pedimos la media de percepción de diferencia de ingresos por región
aggregate(CEP$dif_ing, by=list(CEP$region_factor), mean, na.rm=T)
#Ahora las desviaciones estandar
aggregate(CEP$dif_ing, by=list(CEP$region_factor), sd, na.rm=T)

#Ahora debemos aplicar la prueba de levene para saber si hay igualdad de medias
library(car)
leveneTest(CEP$dif_ing~CEP$region_factor)
#Aplicamos la preuba T, podemos persoanlizar las colas y si se asumen varianzas
#iguales
t.test(CEP$dif_ing~CEP$region_factor,var.equal=T, alternativ="two.sided")
pruebat<-t.test(CEP$dif_ing~CEP$region_factor,var.equal=T, alternativ="two.sided")
pruebat$p.value

#ANOVA
#Primero pedimos la media de percepción de diferencia de ingresos por GSE
aggregate(CEP$dif_ing, by=list(CEP$GSE), mean, na.rm=T)
#Ahora las desviaciones estandar
aggregate(CEP$dif_ing, by=list(CEP$GSE), sd, na.rm=T)

leveneTest(CEP$dif_ing~CEP$GSE)
#Calculamos ANOVA y lo guardamos como objeto
Anova(CEP$dif_ing,CEP$GSE)
anova<-aov(dif_ing~GSE,data = CEP)
summary(anova)
TukeyHSD(anova)
#En un caso donde no hay varianzas iguales podemos usar la prueba welch
#El comando welch.test forma parte del paquete "onewaytests")
oneway.test(dif_ing~GSE,data = CEP)


#prueba Chi cuadrado
chi<-chisq.test(CEP$region_factor,CEP$GSE)
chi$observed #frecuencias observadas
chi$expected #Frecuencias esperadas
chi$stdres #residuos estandarizados


library(corrplot)
corrplot(chi$residuals, is.cor = FALSE)

