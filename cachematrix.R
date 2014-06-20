## The following functions are used to calculate and cache the inverse of a matrix

## the first function 'makeCacheMatrix()' creates a matrix object and defines 4
## functions for setting and retrieving matrix data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) {
                inv <<- inverse
        }
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## the second function 'cacheSolve' checks whether the inverse matrix has
## already been calculated and if so it return the contents of the cache
## if it has not already been calculated and cached, then the fucntion proceeds
## to calculate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
