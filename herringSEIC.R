rm(list=ls(all=TRUE)) #clears workspace

# Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
    dS <- b*(S+E+I+C)*(1-(S+E+I+C)/K) - (mu+f)*S - betaI*S*I - betaC*S*C
    dE <- betaI*S*I + betaC*S*C - (mu+f)*E - lambda*E
    dI <- lambda*E - (mu+alpha+f)*I - rho*I
    dC <- rho*I - (mu+f)*C
    
    return(list(c(dS, dE, dI, dC)))
  })
}

### Set parameters
init       <- c(S= 100, E = 10, I = 10, C = 0)
parameters <- c(betaI = 0.0008, betaC = 0.0004, b = 0.1, mu = 0.001, f = 0.1, lambda = 0.15, alpha = 0.025, rho = 0.167, K = 200)
## Time frame
times      <- seq(0, 1000, by = 0.5)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = seic, parms = parameters)
## change to data frame
out <- as.data.frame(out)

## Show data
head(out, 10)

## Plot
plot(times,out$S)
plot(times,out$E)
plot(times,out$I)
plot(times,out$C)
