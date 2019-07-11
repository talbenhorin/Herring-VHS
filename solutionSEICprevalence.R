rm(list=ls(all=TRUE)) #clears workspace

K = 1000000
gamma = 91.25#E -> I
rho = 30#I -> C
lambda = 0.5#force of infection
b = 1.2#births
mu = 0.15#natural mortality
c <- (b - mu)/K#strength of density dependence
alpha = 2#disease mortality

f <- seq(0,0.15,length=50)

Nstar <- K - f/c
Istar_n <- (gamma*lambda)*(b*K + 2*f*K - (b*f)/c - (f^2)/c - c*K^2) 
Istar_d <- ((mu+f+alpha+rho)*(mu+f+gamma)*(mu+f+lambda))
Istar <- Istar_n/Istar_d
prev <- Istar/Nstar

plot(f,prev)