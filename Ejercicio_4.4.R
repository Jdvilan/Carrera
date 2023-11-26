# 4.4. Simular 500 portafolios y ubicar los siguientes valores de μR y σR cuando
# w =(1/3,1/3,1/3), w = (1, 0, 0), w = (0, 1, 0) y w = (0, 0, 1).

# A) Plantear las matrices

mu_1 <- 0.2
mu_2 <- 0.13
mu_3 <- 0.17
C <- matrix(c(0.0625,0.021,0.0075,0.021,0.0784,0,0.0075,0,0.04),nrow = 3, ncol = 3)
ones <- matrix(c(1,1,1),1)
M <- matrix(c(0.2,0.13,0.17),1)
S <- matrix(c(0.25,0.28,0.2),1)

# B) Generar los portafolios

A <- matrix(runif(1500,min = 0,max = 10),ncol = 3);A
W = A/rowSums(A);W

mu.R <- mu_1*W[,1]+mu_2*W[,2]+mu_3*W[,3];mu.R

risk <- c()

for(i in 1:500){
  risk[i] <- sqrt(W[i,]%*%C%*%t(W)[,i])
}
risk
plot(NULL,xlim = c(0.12,0.3),ylim = c(0.13,0.2),xlab = "Riesgo", ylab = "Rentabilidad")
points(risk,mu.R,pch = 20,cex = 0.5)

# C) Localizar los valores de cada W

w_1 <- matrix(c(1/3,1/3,1/3),1)
mu_R1 <- w_1%*%t(M);mu_R1
s_R1 <- sqrt(w_1%*%C%*%t(w_1));s_R1
points(s_R1,mu_R1,pch = 18,col = "#006DDB",cex = 1.5)

w_2 <- matrix(c(1,0,0),1)
mu_R2 <- w_2%*%t(M);mu_R2
s_R2 <- sqrt(w_2%*%C%*%t(w_2));s_R2
points(s_R2,mu_R2,pch = 18,col = "#FF6DB6",cex = 1.5)

w_3 <- matrix(c(0,1,0),1)
mu_R3 <- w_3%*%t(M);mu_R3
s_R3 <- sqrt(w_3%*%C%*%t(w_3));s_R3
points(s_R3,mu_R3,pch = 18,col = "#490092",cex = 1.5)

w_4 <- matrix(c(0,0,1),1)
mu_R4 <- w_4%*%t(M);mu_R4
s_R4 <- sqrt(w_4%*%C%*%t(w_4));s_R4
points(s_R4,mu_R4,pch = 18,col = "#24FF24",cex = 1.5)
