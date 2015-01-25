#source('./cachematrix.R')

# Generate an invertible matrix
set.seed(123)
n=2000                    # matrix dimension

M <- matrix(runif(n^2),n) # my randomnly generated matrix
#Mi <- solve(A)            # my matrix inverse calculation

# create a cache (list of functions) matrix
cache <- makeCacheMatrix(M)

# 1. test how long it take to calculate the matrix inverse.
# keep in mind first time around the matrix inverse is not cached...
ti1 <- proc.time()
Mi1 <- cacheSolve(cache)
tf1 <- proc.time()
print((tf1-ti1)[1:3])  


# 2. 2nd time around should retrieve it from cache. hence taking much less time...
ti2 <- proc.time()
Mi2 <- cacheSolve(cache)
tf2 <- proc.time()
print((tf2-ti2)[1:3])

# just to confirm these two Matrix inverse are the same...
print(sum(Mi1 != Mi2))
