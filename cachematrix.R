## These functions calculate, cache and return the inverse of a matrix
## to reduce repeated re-calculation. Ideal for use in a loop.

## Creates a special matrix object, implemented as a list containing functions to:
##      1: Set the value of the matrix
##      2: Get the value of the matrix
##      3: Set the value of the cached inverse
##      4: Get the value of the cached inverse
##
## Usage:
##      cm <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
##      cacheSolve(cm)   # computes and caches the inverse
##      cacheSolve(cm)   # returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        if (!is.matrix(x)) {
                stop("argument 'x' must be a matrix")
        }
        if (nrow(x) != ncol(x)) {
                stop("argument 'x' must be a square matrix")
        }

        m <- NULL
        set <- function(y) {
                if (!is.matrix(y)) {
                        stop("argument 'y' must be a matrix")
                }
                if (nrow(y) != ncol(y)) {
                        stop("argument 'y' must be a square matrix")
                }
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special matrix created by makeCacheMatrix().
##      1: If the inverse has already been calculated, it is returned from cache
##         to save computation time.
##      2: Otherwise, calculates the inverse using solve(), stores it in the
##         cache, and returns it.
##
## Note: The matrix must be square and invertible. An error is raised if the
##       matrix is singular (non-invertible).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- tryCatch(
                solve(data, ...),
                error = function(e) {
                        stop("matrix is singular and cannot be inverted: ", conditionMessage(e))
                }
        )
        x$setinverse(m)
        m
}
