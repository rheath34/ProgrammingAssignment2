## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). This pair of functions
## cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # xx stores the inverse matrix
        xx <- NULL
        set <- function(y) {
                x <<- y
                xx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xx <<- inv
        getinv <- function() xx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xx <- x$getinv()
        if(!is.null(xx)) {
                message("getting cached data")
                return(xx)
        }
        data <- x$get()
        xx <- solve(data, ...)
        x$setinv(xx)
        xx
}

