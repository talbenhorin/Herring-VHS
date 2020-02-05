rm(list=ls(all=TRUE)) #clears workspace

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic.pulse <- function(t, x, params) {
  qS <- params["qS"]
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
  b <- k.*exp(-s*cos(pi*t-phi)^2);

  dS <- (b-c*(S+E+I+C))*(S+E+I+C) - (mu+qS*f)*S - beta*S*(I/(S+E+I+C)) 
  dE <- beta*S*(I/(S+E+I+C)) - (mu+qS*f+gamma)*E
  dI <- gamma*E - (mu+qS*f+alpha+rho)*I
  dC <- rho*I - (mu+qC*f)*C  
  list(c(dS,dE,dI,dC))
}

## Set parameters, initial states, and time frame  
params <- c(qS=0.1,qC = 1,gamma = 91.25, rho = 30, beta = 45, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 2,f = 0, s = 50, phi = -1.5708, k = 3.98)#parameter string
xstart <- c(S= 1000000, E = 100, I = 0, C = 0)
times <- seq(0, 50, by = 7/365)

out <- as.data.frame(
    ode(
        func = seic.pulse,
        y = xstart,
        times = times,
        parms = params
        )
    )


