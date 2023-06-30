library(readxl)
ENS <- read_excel("ENS.xlsx")

# ¿Se puede afirmar a partir de la muestra que el nivel medio de colesterol 
# (COLES) supera los 195 mg/hL? Asuma que el nivel de colesterol tiene 
# distribución aproximadamente normal. Utilizar α = 5%.


library(TeachingDemos)

# Queremos saber si son mayores a 195 siempre.

///////////////////////
//PRUEBA DE HIPOTESIS//
///////////////////////


t.test(ENS$COLES, mu=195, alternative="greater")
# los textos son requeridos
# Aquí el p value es 0.6574, que es mayor a 0.05 por lo que se rechaza ha.

# ¿Existe evidencia estadística para mostrar que las personas de esta población
# tienen un nivel medio de presión arterial diastólica (PAD) distinto de 75 
# mg/hL? Asuma que la variable señalada tiene distribución aproximadamente 
# normal. Utilizar α = 5%.

# veamos si puede ser distinto de 75
t.test(ENS$PAD, mu=75, alternative="two.sided")
# siendo que mi p-value es menor a 5%, tenemos que es distinto de 75.
# se apoya ha

# Se postula que la desviación estándar del nivel de colesterol de alta 
# densidad (HDL) es menor a 13 mg/hL. ¿Se puede respaldar dicha información 
# con los datos entregados? Asuma que la variable señalada tiene distribución 
# aproximadamente normal. Utilizar α = 5%.

# H0 = sigma >= 13mg/hl
# Ha = sigma < 13mg/hl

sigma.test(ENS$HDL, sigma=13, alternative="less")
# Tenemos p = 0.01535 < a, por lo que se rechaza h0

// PARA MULTIPLES DISTRIBUCIONES //

# Considere el grupo de mujeres (SEXO = 2) y el grupo de hombres (SEXO = 1) por 
# separado. ¿Se puede afirmar que los niveles de colesterol de alta densidad 
# (HDL) tienen varianzas diferentes entre mujeres y hombres? Asuma que el nivel 
# de colesterol tiene distribución aproximadamente normal. Utilizar α = 5%.

# H0 = colesteroles iguales
# Ha = colesteroles distintos

library(dplyr)

H <- filter(ENS, SEXO == 1)
M <- filter(ENS, SEXO == 2)

var.test(M$HDL, H$HDL, alternative = "two.sided")

# Me da valor p de 25%, así que no se rechaza H0

# Teniendo en cuenta la conclusión en (4), ¿existe evidencia estadística que 
# demuestre que el nivel medio de HDL es mayor en mujeres que en hombres? Asuma 
# que el nivel de colesterol tiene distribución aproximadamente normal. 
# Utilizar α = 5%.

# H0: M <= H
# Ha: M > H

t.test(M$HDL, H$HDL, var.equal = FALSE, alternative = "greater", mu=0)[3]

// PROPORCIÓN //

# ¿Se puede afirmar a partir de la muestra que la mayoría de la población no
# fuma? (variable FUMADOR) Utilizar α = 5%.

# Queremos fumadores sobre población
fumadores <- filter(ENS, FUMADOR == 1)
n_fumadores <- length(fumadores$FUMADOR)
poblacion <- length(ENS$FUMADOR)
# ojo que hay que agregar ese $fumador al final

prop.test(n_fumadores, poblacion, 0.5, alternative = "less", correct = "T")[3]
# Si, estoy bastante seguro (nivel de confianza de 99.99999999%)

# Se afirma que más de un 10% de la población tiene diabetes. ¿Puede respladar
# esta hipótesis con los datos? Utilizar α = 5% y variable DIABETES.

diabeticos <- length(filter(ENS, DIABETES == 1)$DIABETES)

prop.test(diabeticos, poblacion, 0.1, alternative = "greater")

# no se puede respaldar (p value de 0.297)

/////////////////////////
//TEST BONDAD DE AJUSTE//
/////////////////////////

# Realizar un test de bondad de ajuste Log-Normal al nivel de colesterol 
# (COLES). ¿Se puede asumir una distribución Log-Normal? Utilizar significancia 
# de 5%. Ayuda: estimar parámetros por máxima verosimilitud, luego realizar 
# test Kolmogorov-Smirnov (K-S).

fitdist(ENS$COLES, "lnorm", "mle")
ks.test(ENS$COLES, "plnorm", meanlog=5.2473349, sdlog=0.2214904)

#Acá se tiene que ignorar el warning. La p es obligatoria.
# p-value = 0.08448

# Realizar un test de bondad de ajuste Gamma al nivel de colesterol de baja 
# densidad (LDL). ¿Se puede asumir una distribución Gamma? Utilizar 
# significancia de 5%. Ayuda: estimar parámetros por máxima verosimilitud, 
# luego realizar test Kolmogorov-Smirnov (K-S)

fitdist(ENS$LDL, "gamma", "mle")
ks.test(ENS$LDL, "pgamma", shape=10.51812301, rate=0.08914218)

# p-value = 0.3898 no se puede asumir gamma.