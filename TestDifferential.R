## Test Differential Equations

library(deSolve)
model <- function (time, y, parms) {
  with(as.list(c(y, parms)), {
    dN <- r * N * (1 - N / K)
    list(dN)
  })
}
y <- c(N = 0.1)
parms <- c(r = 0.1, K = 10)
times <- seq(0, 100, 1)
out <- ode(y, times, model, parms)
plot(out)
