rm(list=ls(all=TRUE)) #clears workspace

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- (b-c*(S+E+I+C))*(S+E+I+C) - (mu+qS*f)*S - beta*S*(I/(S+E+I+C)) 
    dE <- beta*S*(I/(S+E+I+C)) - (mu+qS*f+gamma)*E
    dI <- gamma*E - (mu+qS*f+alpha+rho)*I
    dC <- rho*I - (mu+qC*f)*C
    
    return(list(c(dB, dS, dE, dI, dC)))
  })
}

## Set parameters and time frame
init       <- c(S= 1000000, E = 100, I = 0, C = 0)
times      <- seq(0, 50, by = 0.5)