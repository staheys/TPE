library(boot)

income <- c(25, 18, 41, 33, 71)


A <- rbind(
  c(1, 1, 1, 1, 1),
  c(2.1, 3.9, 2.4, 2.2, 2.7),
  c(8.6, 9.3, 6.8, 3.9, 6.4),
  c(-0.15, 0, 1, 1, 0),
  c(0, 0, -0.5, 1, 0)
)
b <- c(15000, 42875, 478000, 0, 0)


Aeq <- rbind(
  c(-0.25, 1, 0, 0, 0),
  c(0, 0, 0, 0, 1)
)
beq <- c(0, 580)

result <- simplex(income, A1 = A, b1 = b,
                  A3 = Aeq, b3 = beq, maxi = TRUE)

if (result$solved == 1) {
  cat("Optimal square (x1, x2, x3, x4, x5):\n")
  print(round(result$soln, 2))
  cat("Max:", round(result$value, 2), "thousand. rub.\n")
} else {
  cat("Answer not found:", result$solved, "\n")
}
