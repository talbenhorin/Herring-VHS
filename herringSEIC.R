rm(list=ls(all=TRUE)) #clears workspace

# Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
    dS <- (b-c*(S+E+I+C))*(S+E+I+C) - (mu+f+lambda)*S 
    dE <- lambda*S - (mu+f+gamma)*E
    dI <- gamma*E - (mu+f+alpha+rho)*I
    dC <- rho*I - (mu+f)*C
    
    return(list(c(dS, dE, dI, dC)))
  })
}

### Set parameters
init       <- c(S= 1000000, E = 100, I = 0, C = 0)
parameters <- c(gamma = 91.25, rho = 30, lambda = 0.5, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 2,f = 0.15)

## Time frame
times      <- seq(0, 50, by = 0.5)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = seic, parms = parameters)
## change to data frame
out <- as.data.frame(out)

## Show data
head(out, 10)

## Plot
plot(times,out$S+out$E+out$I+out$C)

