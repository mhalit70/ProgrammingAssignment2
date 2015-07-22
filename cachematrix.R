# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix (set)
# 2. get the value of the matrix (get)
# 3. set the value of inverse of the matrix (setInv)
# 4. get the value of inverse of the matrix (getInv)

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  
  set <- function (y) {
    x <<- y
    mx <<- NULL
  }
  
  get <- function() x
  setInv <- function(solve) mx <<- solve
  getInv <- function() mx
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setInv function.

# This function assumes that the matrix is always invertible. 
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  mx <- x$getInv()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setInv(mx)
  mx
}

## Run a test:
## > x = cbind(c(1,1,0), seq(1:3), c(0,1,2))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
##[1,]    1    1    0
##[2,]    1    2    1
##[3,]    0    3    2

## No cache in the first run
## > cacheSolve(m)
##      [,1] [,2] [,3]
##[1,]   -1    2   -1
##[2,]    2   -2    1
##[3,]   -3    3   -1

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2] [,3]
##[1,]   -1    2   -1
##[2,]    2   -2    1
##[3,]   -3    3   -1
## > 
