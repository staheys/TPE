options(digits = 10, width = 90)
alpha <- 0.05
# Tables contain the rows required for this design.
write.csv(data.frame(N = 8, f = 3, critical = 0.4377),
          "cochran.csv", row.names = FALSE)
write.csv(data.frame(f1 = 2, f2 = 24,
                     critical = qf(1-alpha, 2, 24)),
          "fisher.csv", row.names = FALSE)

check.Cochrans.criterion <- function(criterion, y) {
  tab <- read.csv("cochran.csv")
  hit <- tab$N == nrow(y) & tab$f == ncol(y)-1
  if (sum(hit) != 1L) stop("No matching Cochran value")
  critical <- tab$critical[hit]
  cat("G critical =", critical, "\n")
  return(criterion < critical)
}

check.Fishers.criterion <- function(criterion,
                                    significance.coefs, y) {
  f1 <- nrow(y) - length(significance.coefs)
  f2 <- nrow(y) * (ncol(y)-1)
  if (f1 <= 0) stop("No lack-of-fit degrees of freedom")
  tab <- read.csv("fisher.csv")
  hit <- tab$f1 == f1 & tab$f2 == f2
  if (sum(hit) != 1L) stop("No matching Fisher value")
  critical <- tab$critical[hit]
  cat("F critical =", critical, "\n")
  return(criterion < critical)
}

z.min <- c(198, 83, 7.2)
z.max <- c(239, 200, 23)
z0 <- (z.min + z.max) / 2
delta <- (z.max - z.min) / 2
y <- as.data.frame(matrix(c(
  42,107,64,44, 193,116,137,146,
  294,229,271,244, 112,137,115,160,
  222,224,277,236, 87,137,66,86,
  115,168,185,116, 34,45,47,57
), nrow = 8, byrow = TRUE))
names(y) <- paste0("y", 1:4)
plan <- expand.grid(x1 = c(-1, 1),
                    x2 = c(-1, 1), x3 = c(-1, 1))
X <- with(plan, cbind(b0 = 1, b1 = x1, b2 = x2,
                      b3 = x3, b12 = x1*x2, b13 = x1*x3,
                      b23 = x2*x3, b123 = x1*x2*x3))
N <- nrow(y); m <- ncol(y)
y.mean <- rowMeans(y)
s2 <- apply(y, 1, var)
s2.rep <- mean(s2)
G <- max(s2) / sum(s2)
b <- drop(crossprod(X, y.mean) / N)
sb <- sqrt(s2.rep / (N*m))
t.value <- abs(b) / sb
t.critical <- qt(1-alpha/2, N*(m-1))
significant <- t.value > t.critical
significance.coefs <- b[significant]
y.hat <- drop(X[, significant, drop = FALSE] %*%
                significance.coefs)
SSE <- sum((y.hat-y.mean)^2)
s2.ad <- m*SSE / (N-sum(significant))
F.value <- s2.ad / s2.rep
reproducible <- check.Cochrans.criterion(G, y)
adequate <- check.Fishers.criterion(F.value,
                                    significance.coefs, y)
cat("R version:", R.version.string, "\n")
cat("z0:", z0, "\ndelta:", delta, "\n")
print(data.frame(run = 1:N, mean = y.mean, variance = s2))
print(data.frame(b, t = t.value, significant))
cat("S2_rep =", s2.rep, "\nG =", G,
    "\ns_b =", sb, "\nt critical =", t.critical,
    "\nSSE =", SSE, "\nS2_ad =", s2.ad,
    "\nF =", F.value, "\n")
print(data.frame(run = 1:N, mean = y.mean,
                 predicted = y.hat, residual = y.mean-y.hat))
cat("Reproducible:", reproducible,
    "\nAdequate:", adequate, "\n")
b.manual <- c(141.03125, -36.34375, 4.53125, -9.65625,
              -20.84375, -25.15625, -40.03125, 32.21875)
stopifnot(max(abs(b-b.manual)) < 1e-10,
          abs(s2.rep-780.71875) < 1e-10,
          abs(SSE-910.203125) < 1e-10,
          reproducible, adequate)
cat("Manual comparison: PASS\n")
