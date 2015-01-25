##  to avoid re-calculating the inverse of a square matrix, we cache the result 
# of that calculation when it's first performed. We then reuse that cached value 
# in subsequent calls. 

## makeCacheMatrix receives a matrix and provides enclosed list of functions 
# to effect actions over that matrix namely to  
# store the result in an environment variable (mi).

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) mi <<- solve
  getmatrixinv <- function() mi
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}


## cachesolve is a special case of solve uses a cached matrix .
## Checks the cache : x, to confirm if the inverse of a matrix (X) has been calculated already
## if yes, then use that inverse, otherwise, will calculate using solve
## and forward that value/function call to the cache matrix environment variable.
## next time around it is called , will exist in cache, avoiding re-calculation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getmatrixinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setmatrixinv(mi)
  mi
}

### TESTS

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

