## Functions used for creating an object that caches a matrix as well as its
## inverse once computed.

## Usage:
##      1) Create a "cached matrix" object using makeCacheMatrix()
##      2) Calculate the inverse of the matrix using cacheSolve()
## If the inverse of the same matrix is needed again, calling cacheSolve will
## return the previously calculated inverted matrix.
##
## Example:
##      m <- matrix(c(2, 0, 0, 2), 2, 2)
##      x <- makeCacheMatrix(m)
##      inv1 <- cacheSolve(x)   # Inverse computed with solve(), then cached
##      inv2 <- cacheSolve(x)   # Cached inverted matrix will be returned

## Creates a special "matrix" object that can cache its inverse.
## Argument:
##     x    a square numeric or complex matrix
##          assumed always to be invertible
makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inv) {
        x_inverse <<- inv
    }
    
    getInverse <- function() {
        x_inverse
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve() retrieves the inverse from the cache.
## Arguments:
##      x       "cached matrix" object created with makeCacheMatrix()
##      ...     optional arguments passed to solve()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Inverse already calculated, return the cached matrix
    if (!is.null(m)) {
        return(m)
    }

    ## No inverse cached, calculate and cache it
    m <- x$get()
    m <- solve(m, ...)
    x$setInverse(m)
    
    m
}
