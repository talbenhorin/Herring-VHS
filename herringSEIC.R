rm(list=ls(all=TRUE)) #clears workspace

## Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

## Load deSolve package
library(deSolve)

## Create a EIC function
eic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
    dS <- b*(S+E+C) - (mu+f)*S - betaI*S*I - betaC*S*C
    dE <- betaI*S*I - betaC*S*C - (mu+f)*E - lambda*E
    dI <- lambda*E - (mu+alpha+f)*I - rho*I
    dC <- rho*I - (mu+alpha+f)*C
    
    return(list(c(dS, dE, dI, dC)))
  })
}

### Set parameters
init       <- c(S= 100, E = 10, I = 10, C = 0)
## b: reproduction; beta: transmission; mu: natural mortality; r: WS mortality; c: parasite shedding; gamma: parasite loss
parameters <- c(betaI = 0.0008, betaC = 0.0004, b = 0.1, mu = 0.001, f = 0, lambda = 0.15, alpha = 0.025, rho = 0.167)
## Time frame
times      <- seq(0, 1000, by = 0.5)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = eic, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
plot(times,(out$E+out$I)/(out$S+out$E+out$I+out$C))