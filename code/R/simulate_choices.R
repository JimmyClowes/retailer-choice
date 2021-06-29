N <- 500 # Number of individuals
R <- 5 # Number of retailers
C <- 4 # Number of characteristics
P <- 10 # Number of product types

# Generate covariance matrix
num_gen_vars <- R+C

set.seed(456)
A <- matrix(runif(num_gen_vars^2)*2-1, ncol=num_gen_vars) 
Sigma <- t(A) %*% A
cov2cor(Sigma)

set.seed(789)
prefs <- MASS::mvrnorm(n = N,
                       mu = rep(0,num_gen_vars),
                       Sigma = Sigma)

pairs(prefs)

prefs_df <- data.frame(prefs)

# Assign characteristics
names(prefs_df)[seq(R+1, R+C, 1)] <- paste0("char",c(1:C))

prefs_df$char1 <- cut(prefs_df$char1, breaks=c(-Inf, 0, Inf), labels = c("M", "F"))
prefs_df$char2 <- cut(prefs_df$char2, breaks=c(-Inf, -1.5, 1.5, Inf), labels = c("Young", "Middle", "Old"))
prefs_df$char3 <- cut(prefs_df$char3, breaks=c(-Inf, -1.5, 1.5, Inf), labels = c("Low", "Medium", "High"))
prefs_df$char4 <- cut(prefs_df$char4, breaks=c(-Inf, -1.5, 0, 1.5, Inf), labels = c("N", "S", "E", "W"))

require(ggplot2)
require(GGally)
ggpairs(prefs_df) + theme_classic()

# Determine which retailers offer which product types
retailer_prodstock <- list()
min_types_stocked = 3
for(i in c(1:R)){
  num_stocked <- sample(c(min_types_stocked:P), 1, replace = FALSE)
  prods_stocked <- sample(c(1:P), num_stocked, replace = FALSE)
  retailer_prodstock[[i]] <- sort(prods_stocked)
}
