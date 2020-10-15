rm(list=ls(all=TRUE)) #clears workspace

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic.pulse <- function(t, x, params) {
  qS <- params["qS"]
  qC <- params["qC"]
  gamma <- params["gamma"]
  rho <- params["rho"]
  beta <- params["beta"]
  mu <- params["mu"]
  c <- params["c"]
  alpha <- params["alpha"]
  f <- params["f"]
  s <- params["s"]
  k <- params["k"]
  phi <- params["phi"]
  b <- k*exp(-s*cos(pi*t-phi)^2)
  
  dS <- (b-c*(x[1]+x[2]+x[3]+x[4]))*(x[1]+x[2]+x[3]+x[4]) - (mu+qS*f)*x[1] - beta*x[1]*(x[3]/(x[1]+x[2]+x[3]+x[4])) 
  dE <- beta*x[1]*(x[3]/(x[1]+x[2]+x[3]+x[4])) - (mu+qS*f+gamma)*x[2]
  dI <- gamma*x[2] - (mu+qS*f+alpha+rho)*x[3]
  dC <- rho*x[3] - (mu+qC*f)*x[4]  
  dH <- qS*f*x[1]+qS*f*x[2]+qS*f*x[3]+qC*f*x[4]
  list(c(dS,dE,dI,dC))
}

## Set parameters, initial states, and time frame  
params <- c(qS=0.1,qC = 1,gamma = 91.25, rho = 30, beta = 45, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 2,f = 0, s = 50, phi = -1.5708, k = 3.98)#parameter string
xstart <- c(S= 100000, E = 100, I = 0, C = 0)
times <- seq(0, 50, by = 0.01917808)

out <- as.data.frame(
    ode(
        func = seic.pulse,
        y = xstart,
        times = times,
        parms = params
        )
    )

op <-par(fig=c(0,1,0,1),mfrow=c(2,2),
         mar=c(3,3,1,1),mgp=c(2,1,0))

plot(S~time,data=out,type='l')
plot(E~time,data=out,type='l')
plot(I~time,data=out,type='l')
plot(C~time,data=out,type='l')

