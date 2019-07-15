rm(list=ls(all=TRUE)) #clears workspace

# Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

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

eqsize.cold <- numeric(length(fishing)) # a vector to hold the solutions
eqsize.warm <- numeric(length(fishing)) # a vector to hold the solutions

for(i in seq_along(fishing)){
  p.cold <- c(qS = 0.1,qC = 1,gamma = 91.25, rho = 30, beta = 45, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 2,f = fishing[i])
  p.warm <- c(qS = 0.1,qC = 1,gamma = 91.25, rho = 91.25, beta = 45, b = 0.6, mu = 0.15, c = 1.05e-06,alpha = 0.1,f = fishing[i])
  out.cold <- as.data.frame(
    out.cold <- ode(
      y = init, 
      times = times, 
      func = seic, 
      parms = p.cold
      )
    )
  out.warm <- as.data.frame(
    out.warm <- ode(
      y = init, 
      times = times, 
      func = seic, 
      parms = p.warm
    )
  )
  eqsize.warm[i] <- tail(out.warm$S+out.warm$E+out.warm$I+out.warm$C,1)
  eqsize.cold[i] <- tail(out.cold$S+out.cold$E+out.cold$I+out.cold$C,1)
}

plot(fishing,eqsize.warm,type='l',col='red')
points(fishing,eqsize.cold,type='l',col='blue')
