
f <- function(x) exp(x - 3) - x^2

a <- 5
b <- 6
eps <- 0.0001

F <- c(1, 1)
while (tail(F, 1) < (b - a) / eps) {
  F <- c(F, sum(tail(F, 2)))
}

n <- length(F)

x1 <- a + F[n-2] / F[n] * (b-a)
x2 <- a + F[n-1] / F[n] * (b-a)

for (i in 1:(n-3)) {
  if (f(x1) <= f(x2)) {
    b <- x2
    x2 <- x1
    x1 <- a + F[n-i-2] / F[n-i] * (b-a)
  } else {
    a <- x1
    x1 <- x2
    x2 <- a + F[n-i-1] / F[n-i] * (b-a)
  }
}

if (f(x1) <= f(x1 + eps/10)) {
  b <- x1 + eps/10
} else {
  a <- x1
}

x_min <- (a + b) / 2

cat("min x =", x_min, "\n")
cat("function value =", f(x_min), "\n")
cat("iterations =", n-2, "\n")

curve(f(x), from=5, to=6)
points(x_min, f(x_min), col="red", pch=19)
