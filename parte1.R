# Considere X~Poisson(7) y Y|X~Binomial(x,0.3)

//////////////////////////////////////////////
//DISTRIBUCIONES DE PROBABILIDAD Y CONJUNTAS//
//////////////////////////////////////////////

# Siendo que x determina la cantidad de intentos que habrán en la binomial,
# se podría calcular la probabilidad merginal de y como una Poisson con
# el intervalo modificado por la probabilidad de la binomial:
7*(0.3)
# = 2.1
# Con eso tenemos que y marginal: Y~Poisson(2.1)

# Calcule P(X = 7, Y = 4)
# Tenemos que X e Y no son independientes eso si.
dpois(7,7) * dbinom(4,7,0.3)

# Calcule P(Y ≤ 4|X = 9)
pbinom(4,9,0.3)

# Calcule P(X = 10|Y = 7)
# Cómo diablos calculo x dado y?
# P(x|y) = P(y|x)*P(x)/P(y)
dbinom(7,10,0.3) * dpois(10,7) / dpois(7,2.1)

# Calcule P(X ≤ 10|Y = 7)
dbinom(7,10,0.3) * dpois(10,7) / dpois(7,2.1)
# Acabo de cachar que acá hay que usar un for
X <- 0:10
sum(dbinom(7,X,0.3) * dpois(X,7)) / dpois(7,2.1)

# Considerando el modelo conjunto
# X ∼ Binomial(12, 0.7) , Y |X = x ∼ Binomial(x, 0.3)
# Calcule:

# P(Y ≥ 5)
# Para esto tenemos que sacar la marginal de Y:
0.7 * 0.3
# =0.21
# Siendo que acá queremos mayor que le hacemos 1-p
1 - pbinom(5, 12, 0.21)

# P(3 ≤ Y ≤ 7|X = 6)
pbinom(7, 6, 0.3) - pbinom(3, 6, 0.3)
# Ojo porque es discreto, así que hay que usar 2 en lugar de 3:
pbinom(7, 6, 0.3) - pbinom(2, 6, 0.3)

# P(X = 6|Y = 3)
# P(x|y) = P(y|x)*P(x)/P(y)
dbinom(3, 6, 0.3) * dbinom(6, 12, 0.7) / dbinom(3, 12, 0.21)
# Ojo que ese P(y|x) usa el y pedido para sacar probabilidades y que
# el P(y) marginal es igual al anterior.


////////////////////
//NORMAL BIVARIADA//
////////////////////

# Considere:
# (X,Y) ∼ NormalBivariada(µX,µY,σX,σY,ρ)
# con µX = µY = 2, σX = 1, σY = 2, y ρ = 0.3. 
# Calcule:
# P(1 < X < 2; 1 < Y < 3)

# Esto requiere mvtnorm:
library(mvtnorm)
medias <- c(2, 2)
matrizCov <- matrix(c(1**2, rep(0.3, 1, 2, 2), 2**2),2,2)

pmvnorm(lower = c(1,1), upper = c(2,3),
        mean = medias, sigma = matrizCov)[1]

#  P(X < 2.3)
pmvnorm(lower = c(-Inf,-Inf), upper = c(2.3,Inf),
        mean = medias, sigma = matrizCov)[1]

# P(X > 1; Y < 2.3)
pmvnorm(lower = c(1,-Inf), upper = c(Inf,2.3),
        mean = medias, sigma = matrizCov)[1]


/////////////////////////////////////////////////////
//TEOREMA DEL LIMITE CENTRAL APROXIMADO A LA NORMAL//
/////////////////////////////////////////////////////

# X1, . . . , X60 iid∼ Poisson(4)
# calcular aproximadamente P(prom(X) ≤ 4.2). (Recordar corrección por continuidad)

# Esto quiere decir, si tengo 60 Xs que distribuyen poisson con media 4
# qué probabilidad tengo de que prom(x) < 4.2 ?

# Siendo que tengo muchos datos, prom(x) = normal(mu, sigma/sqrt(n))
pnorm(4.2, 4, sqrt(4)/sqrt(60))
# Tengo que hacer un ajuste por continuidad, que básicamente es sumarle o
# restarle 0,5 a todos los datos por separado:
pnorm(4.2 + 0.5/60, 4, sqrt(4)/sqrt(60))
# En este caso se suma porque (?), debería estar bien también la resta, quiza.

# Si Y1, . . . , Y45 iid∼ Normal(4, 1), calcular P(3 < prom(Y) < 4.2).
# Acá no se tiene que hacer ajuste por continuidad:
pnorm(4.2, 4, 1/sqrt(45)) - pnorm(3, 4, sqrt(1)/sqrt(45))

# Si Z1, . . . , Z40 iid∼ Gamma(2, 2), calcular aproximadamente P(prom(Z) > 1.1).
1 - pnorm(1.1, 1, sqrt(1/2)/sqrt(40))

# Si W1, . . . , W120 iid∼ Bernoulli(0.2), calcular aproximadamente 
# P(prom(W) ≥ 0.25). (Recordar corrección por continuidad)
1 - pnorm(0.25, 0.2, sqrt(0.2*(1-0.2))/sqrt(120))
# Ajustado por continuidad:
1 - pnorm(0.25-0.5/120, 0.2, sqrt(0.2*(1-0.2))/sqrt(120))
# Se resta porque estamos restando en un principio, y p*(1-p) es la varianza
# de la bernoulli


/////////////////////
//MÁXIMOS Y MINIMOS//
/////////////////////

# Si X1, . . . , X5 iid∼ Uniforme(3, 6), calcular la probabilidad 
# P(X(5) > 5.5), donde X(5) = max{X1, . . . , X5}.

# Queremos calcular el máximo de un evento probabilistico. Eso se saca como
# la probabilidad de que ese evento pase/no pase en n veces, es decir a la n.
1 - punif(5.5, 3, 6)^5

# Si (T1, . . . , T10) iid∼ Exponencial(0.5), calcular P(T(10) < 2), 
# donde T(10) = max{T1, . . . , T10}.

# Queremos la probabilidad de que el máximo sea menor a 2:
pexp(2, 0.5)^10

# Si (S1, . . . , S4) iid∼ Normal(0, 2), calcular la probabilidad
# P(S(1) > −0.3), donde S(1) = min{S1, . . . , S4}

# Queremos la probabilidad de que el mínimo sea mayor a -0.3.
(1-pnorm(-0.3, 0, 2))^4

# Si W1, . . . , W14 iid∼ Beta(3, 4), calcular P(W(1) ≤ 0.15), 
# donde W(1) = min{W1, . . . , W14}

# Queremos la probabilidad de que el minimo sea menor a 0.15
1 - (1-pbeta(0.15, 3, 4))^14

# Ahora vamos a usar ENS:
library(readxl)
ENS <- read_excel("ENS.xlsx")


////////////////////////////////////////////
//AJUSTE DE COEFICIENTES DE DISTRIBUCIONES//
////////////////////////////////////////////

// POR GRÁFICO //

# Para la variable Colesterol (COLES), ajustar por gráfico de probabilidad una 
# distribución LogNormal(λ, ζ).

# Veamos el colesterol ordenado
coles <- sort(ENS$COLES)
coles

# lm(x ~ y) es para ajustar linealmente x con y. Nosotros queremos COLES con
# una LogNormal, así que podemos aprovecharnos de que lm sirve para vectores:
vector_1_len <- 1:length(coles)
m = vector_1_len/(length(coles)+1)
lm(log(coles)~qnorm(m))$coefficients

# Importante tirar el coefficients para que te de precisa la cosa

# Ajustar una distribución Log-Logística(µ, σ) al nivel de colesterol de baja
# densidad (LDL) por medio de gráfico de probabilidad.

# Usaremos la siguiente notación:
x <- sort(ENS$LDL);
n <- length(x)
m <- 1:n
p <- m/(n+1)
lm(log(x) ~ qlogis(p))$coefficients

# Ajustar una distribución Logística(µ, σ) a la presión arterial diastólica 
# (PAD) por método de máxima verosimilitud.

// POR MAX VEROSIMILITUD //

# Para esto usaremos fitdeeznutz
library(fitdistrplus)

fitdist(ENS$PAD, "logis", "mle")
# Esto en "estimate" me da 75.889020 y 6.449585

# Para la variable Colesterol (COLES), ajustar por máxima verosimilitud una 
# distribución Gamma(k, ν)

// POR MOMENTOS //

fitdist(ENS$COLES, "gamma", "mle")
# 20.8023593, 0.1068317

# Estimar los parámetros de una distribución Gamma(k, ν) para ajustar a la 
# presión arterial sistólica (PAS) por medio de método de momentos.

fitdist(ENS$PAS, "gamma", "mme")

# Ajustar una distribución Weibull(β, η) por máxima verosimilitud al nivel de 
# colesterol de alta densidad (HDL).

fitdist(ENS$HDL, "weibull", "mle")

# El resto de los problemas es irrelevante