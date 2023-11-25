# 2.4. Simular 1000 portafolios con los datos ofrecidos y presentar resultados 
# en gráfica.

mu_1 <- 0.08
mu_2 <- 0.05
s_1 <- 0.07
s_2 <- 0.04
s_12 <- 0.25

A <- matrix(runif(2000,min = 0,max = 10),ncol = 2);A
W = A/rowSums(A);W

# A) Retorno esperado del portafolio 

mu.R <- mu_1*W[,1]+mu_2*W[,2];mu.R

# B) Riesgo del retorno

var_R <- function(rho){
  return(matrix(c(s_1^2,rho*s_1*s_2,rho*s_1*s_2,s_2^2),2))
}

risk <- c()

for(i in 1:1000){
  risk[i] <- sqrt(W[i,]%*%var_R(0.25)%*%t(W)[,i])
}

plot(NULL,xlim = c(0.03,0.08),ylim = c(0.04,0.09),xlab = "Riesgo", ylab = "Rentabilidad")
points(risk,mu.R,pch = 20,cex = 0.5)

# Ejercicio 2.1
Mu.R <- 0.065
M.risk <- 0.044
points(M.risk,Mu.R,pch = 22,col = "#FF0000",cex = 1.5)

# Ejercicio 2.2
points(s_1,mu_1,pch = 22,col = "#33FF00",cex = 1.5)

# Ejercicio 2.3
w.opt <- (s_2^2-s_12*s_1*s_2)/(s_1^2+s_2^2-2*s_12*s_1*s_2)
mu.rop <- mu_1*w.opt+mu_2*(1-w.opt);mu.rop
w.o <- matrix(c(w.opt,1-w.opt),1);w.o
sigma.R <- sqrt(w.o%*%var_R(0.25)%*%t(w.o));sigma.R
points(sigma.R,mu.rop,pch = 22,col = "#00FFFF",cex = 1.5)

