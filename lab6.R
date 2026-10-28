
# Laboratory work 7.6, variant 3

# Read data
a <- read.table("iteration_0.txt", header=TRUE, sep=";")
b <- read.table("iteration_1.txt", header=TRUE, sep=";")
c <- read.table("iteration_2.txt", header=TRUE, sep=";")

# Four plots in one window
par(mfrow=c(2,2))

# Iteration 0
plot(a$cars, a$particles, main="Iteration 0",
     xlab="Cars", ylab="Particles")

# Iteration 1
plot(b$cars, b$particles, main="Iteration 1",
     xlab="Cars", ylab="Particles")

# Iteration 2
plot(c$cars, c$particles, main="Iteration 2",
     xlab="Cars", ylab="Particles")

# All iterations
plot(a$cars, a$particles, main="All iterations",
     xlab="Cars", ylab="Particles",
     xlim=c(0,100), ylim=c(0,105))

points(b$cars, b$particles, pch=2)
points(c$cars, c$particles, pch=3)

legend("topleft", c("0", "1", "2"), pch=c(1,2,3))
