## cachematrix provides functions to evaluate the inverse of a numeric matrix,
## and to cache the result for subsequent evaluations.

## Encapsulates an inversable numeric matrix and provides functions
## to get and set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Returns the inverse of a numeric matrix encapsulated by makeCacheMatrix.
## Uses cached data if set; otherwise, solves for the matrix and caches the result.
cacheSolve <- function(x, ...) {
        message("solving matrix")
        i <- x$getinverse()

        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()

        i <- solve(data, ...)
        x$setinverse(i)
        i
}
