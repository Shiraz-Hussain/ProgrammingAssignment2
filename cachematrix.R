## makeCacheMatrix and cacheSolve functions reduce the time required to calculate a matrix inverse by storing the inverse of
## a matrix in a "special" matrix.

## makeCacheMatrix returns a "special" matrix that caches the inverse of a matrix x, that is its argument.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve_inv) inv <<- solve_inv
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## cacheSolve returns a matrix that is the inverse of 'x' from cache matrix if x has not changed. Otherwise, recalculates the inverse and
## returns it while also updating the cache matrix.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv      
}
