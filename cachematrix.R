## These functions allow the program to run more efficiently by
## caching certain computations that maybe costly. In this case,
## it caches the inverse of a matrix to avoid computing again.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(inv) {
    invX <<- inv
  }
  getInv <- function() {
    invX
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x$getInv()
  if(!is.null(mat)) {
    mat
  }
  data <- x$get()
  mat <- solve(data)
  x$setInv(mat)
  mat
}
