rm(list=ls(all=TRUE)) #clears workspace

# Install these packages if you haven't already
install.packages("Rtools")
install.packages("deSolve")

# Load deSolve package
library(deSolve)

# Create an SEIC function
seic <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
    dS <- (b-c*(S+E+I+C))*(S+E+I+C) - (mu+qS*f)*S - beta*S*(I/(S+E+I+C)) 
    dE <- beta*S*(I/(S+E+I+C)) - (mu+qS*f+gamma)*E
    dI <- gamma*E - (mu+qS*f+alpha+rho)*I
    dC <- rho*I - (mu+qC*f)*C
    
    return(list(c(dS, dE, dI, dC)))
  })
}

## Set parameters and time frame
init       <- c(S= 1000000, E = 100, I = 0, C = 0)
fishing <- seq(0,1,length=10)
times      <- seq(0, 50, by = 0.5)

p <- c(qS = 0.1,qC = 1,gamma = 91.25, rho = 30, beta = 45, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 2,f = 0)#parameter string
out <- ode(
  y = init,
  times = times,
  func = seic,
  parms = p
  )

plot(out,type='l',col='blue')
