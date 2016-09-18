## The functions save computing power as calculating the inverse of a matrix can slow down processing, as it is a labour intensive process.
## If the inverse of a matrix is found in the cache, the function will return it, instead of calculating it anew. 

## this creates a matrix that holds the list of functions that get/set the value of the matrix and get/set the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## here, the inverse of the matrix is calculated if the matrix is not found in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}



