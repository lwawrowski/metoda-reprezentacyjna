d <- 0.03
prawd <- 0.95
alfa <- 1-prawd
p0 <- 0.73

z <- qnorm(p = 1-alfa/2)

n_min <- ceiling(z^2*p0*(1-p0)/d^2)

# eksperyment

N <- 535946
m <- round(p0*N)

zadowoleni <- rep(1, m)
niezadowoleni <- rep(0, N-m)

populacja <- c(zadowoleni, niezadowoleni)

summary(populacja)

n <- n_min

proba <- sample(x = populacja, size = n)

mean(proba)

# symulacja

wyniki <- numeric(1000)

for(symulacja in 1:1000){
  
  proba <- sample(x = populacja, size = n)
  
  wyniki[symulacja] <- mean(proba)
  
}

summary(wyniki)

mean(wyniki > 0.7 & wyniki < 0.76)











