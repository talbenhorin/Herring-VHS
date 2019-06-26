rm(list=ls(all=TRUE)) #clears workspace

## Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

## Load deSolve package
library(deSolve)

## Create a EIC function
eic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
    dE <-  -lambda*E
    dI <-  lambda*E - alpha*I - rho*I
    dC <-  rho*I
    
    return(list(c(dE, dI, dC)))
  })
}

### Set parameters
init       <- c(E = 31, I = 0, C = 0)
## b: reproduction; beta: transmission; mu: natural mortality; r: WS mortality; c: parasite shedding; gamma: parasite loss
parameters <- c(lambda = 0.25, alpha = 0.025, rho = 0.167)
## Time frame
times      <- seq(0, 27, by = 0.1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = eic, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
plot(times,out$E+out$I+out$C)