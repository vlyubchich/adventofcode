# Day 1 ----
# https://adventofcode.com/2022/day/1
# Read in and convert to a vector by selecting that only column
x <- read.table("./2022/data/input1.txt", blank.lines.skip = FALSE)[, 1]
# Indices where we should stop summing correspond to NAs and just the vector limits
i_break <- c(1, which(is.na(x)), length(x))
# Sum up within each group, disregard NAs using na.rm = TRUE
xsums <- sapply(1:(length(i_break) - 1), function(i)
    sum(x[i_break[i]:i_break[i + 1]], na.rm = TRUE)
)
# The answer
max(xsums)
# [1] 69501

# https://adventofcode.com/2022/day/1#part2
# Select three maximal values
max3 <- sort(xsums, decreasing = TRUE)[1:3]
# The answer
sum(max3)
# [1] 202346


# Day 2 ----
# https://adventofcode.com/2022/day/2
D <- read.table("./2022/data/input2.txt")
# Replace chars for easy matching
D$V2[D$V2 == "X"] <- "A" # Rock
D$V2[D$V2 == "Y"] <- "B" # Paper
D$V2[D$V2 == "Z"] <- "C" # Scissors
# The question says X, Y, Z, but rename right away here
points_per_shape <- c(A = 1, B = 2, C = 3)
tot_per_shape <- sum(points_per_shape[D$V2])
# points_per_outcome <- c(lost = 0, draw = 3, won = 6)
tot_per_outcome <- 3 * sum(D$V1 == D$V2) +
    6 * (sum(D$V1 == "A" & D$V2 == "B") +
             sum(D$V1 == "B" & D$V2 == "C") +
             sum(D$V1 == "C" & D$V2 == "A")
    )
tot_per_shape + tot_per_outcome
# [1] 10310

# https://adventofcode.com/2022/day/2#part2
losing <- c(A = "C", B = "A", C = "B")
winning <- c(A = "B", B = "C", C = "A")
D$V3 <- NA
D$V3[D$V2 == "A"] <- losing[D$V1][D$V2 == "A"]
D$V3[D$V2 == "B"] <- D$V1[D$V2 == "B"]
D$V3[D$V2 == "C"] <- winning[D$V1][D$V2 == "C"]
tot_per_shape <- sum(points_per_shape[D$V3])
tot_per_outcome <- 3 * sum(D$V1 == D$V3) +
    6 * (sum(D$V1 == "A" & D$V3 == "B") +
             sum(D$V1 == "B" & D$V3 == "C") +
             sum(D$V1 == "C" & D$V3 == "A")
    )
tot_per_shape + tot_per_outcome
# [1] 14859


# Day 3 ----
# https://adventofcode.com/2022/day/3
X <- read.table("./2022/data/input3.txt")[, 1]
priority <- 1:52
names(priority) <- c(letters, LETTERS)
tmp <- sapply(X, function(x) {
    y <- unlist(strsplit(x, ""))
    n <- length(y)
    repeated <- intersect(y[1:(n/2)], y[(n/2 + 1):n])
    priority[repeated]
})
sum(unlist(tmp))
# [1] 7845

# https://adventofcode.com/2022/day/3#part2
groups <- rep(1:(length(X)/3), each = 3)
tmp <- tapply(X, groups, function(x) {
    y <- lapply(x, function(i) unlist(strsplit(i, "")))
    Reduce(intersect, y)
})
sum(priority[tmp])
# [1] 2790


# Day 4 ----
# https://adventofcode.com/2022/day/4
X <- read.table("./2022/data/input4.txt", sep = ",")
X1 <- sapply(X[, 1], function(i) as.numeric(unlist(strsplit(i, split = "-"))))
X2 <- sapply(X[, 2], function(i) as.numeric(unlist(strsplit(i, split = "-"))))
# Check if X1 contains X2
c1 <- (X1[1,] <= X2[1,]) & (X1[2,] >= X2[2,])
# Check if X2 contains X1
c2 <- (X2[1,] <= X1[1,]) & (X2[2,] >= X1[2,])
# Check if counted twice, e.g., if pairs are 23-46,23-46
cc <- c1 & c2
sum(c1) + sum(c2) - sum(cc)
# [1] 507

# https://adventofcode.com/2022/day/4#part2
o1 <- ((X1[2,] >= X2[1,]) & (X1[2,] <= X2[2,])) | # right end of X1 is within X2
    ((X1[1,] >= X2[1,]) & (X1[1,] <= X2[2,])) | # left end of X1 is within X2
    ((X1[1,] <= X2[1,]) & (X1[2,] >= X2[2,])) | # X1 covers X2
    ((X2[1,] <= X1[1,]) & (X2[2,] >= X1[2,]))  # X2 covers X1
sum(o1)
# [1] 897

# Day 5 ----
# https://adventofcode.com/2022/day/5
X <- readLines("./2022/data/input5.txt", n = 8)
U <- lapply(X, function(x) {
    xx <- unlist(strsplit(x, ""))
    xx[seq(2, 34, by = 4)]
})
U <- do.call(rbind, U)
# List, first element is on top
X <- apply(U, 2, function(u) {
    u[u != " "]
})

instr <- read.table("./2022/data/input5.txt", skip = 10)
for (i in 1:nrow(instr)) {
    n <- instr$V2[i]
    from <- instr$V4[i]
    to <- instr$V6[i]
    for (k in 1:n) {
        x <- X[[from]][1]
        X[[from]] <- X[[from]][-1]
        X[[to]] <- c(x, X[[to]])
    }
}
res <- sapply(X, function(x) x[1])
paste(res, collapse = "")
# DHBJQJCCW

# https://adventofcode.com/2022/day/5#part2
X <- readLines("./2022/data/input5.txt", n = 8)
U <- lapply(X, function(x) {
    xx <- unlist(strsplit(x, ""))
    xx[seq(2, 34, by = 4)]
})
U <- do.call(rbind, U)
# List, first element is on top
X <- apply(U, 2, function(u) {
    u[u != " "]
})
for (i in 1:nrow(instr)) {
    n <- instr$V2[i]
    from <- instr$V4[i]
    to <- instr$V6[i]
    x <- X[[from]][1:n]
    X[[from]] <- X[[from]][-c(1:n)]
    X[[to]] <- c(x, X[[to]])
}
res <- sapply(X, function(x) x[1])
paste(res, collapse = "")
# WJVRLSJJT



