
r <- c(8, 8.5)

a <- matrix(c(0.2, 0.14, 0.16, 0.5,
              0.4, 0.25, 0.15, 0.2),
            nrow = 2, byrow = TRUE)

w <- r / sum(r)
s <- colSums(a * w)

print(round(s, 4))
cat("Best:", which.max(s), "\n")
