rm(list=ls(all=TRUE)) #clears workspace

K = 500000;
gamma = 0.25;#E -> I
rho = 0.125;#I -> C
lambda = 0.4;#force of infection
b = 1.2;#births
mu = 0.15;#natural mortality
c <- (b - mu)/K;#strength of density dependence
alpha = 1;#disease mortality

seq(0,0.15,length=50)