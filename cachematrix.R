## The below functions compute and cache matrix inverses.
## Use both functions to return the inverse of your input matrix.

## makeCacheMatrix outputs a list of functions used to cache matrix inverses:
## input of makeCacheMatrix is a matrix e.g.:
##  x <- matrix(c(2, 4, 3, 1, 5, 7), nrow=2, ncol=2)
##  makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { # changes matrix x
                x <<- y
                inv <<- NULL
        }
        get <- function() x # gets matrix x
        setInverse <- function(inverse) inv <<- inverse # stores inverse inv
        getInverse <- function() inv # gets inverse inv
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes inverses and retrieves from cache if already computed.
## input of cacheSolve is the object where makeCacheMatrix is stored, e.g.:
##  inputSolve <- makeCacheMatrix(x)
##  cacheSolve(inputSolve)

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) { # verify presence of cached inverse
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # gets matrix stored in makeCacheMatrix
        inv <- solve(data, ...) # calculates inverse
        x$setInverse(inv) # stores inverse in makeCacheMatrix
        inv
}