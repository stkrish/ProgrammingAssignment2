## Put comments here that give an overall description of what your
## functions do

## To create a special matrix object that can cache its inverse

makecacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        ##getmat() and setmat() functions to set and retrieve the matrix
        setmat <- function(matA) m <<- matA
        getmat <- function() m
        list(set = set, get = get,
        setmat = setmat,
        getmat = getmat)
}

# To compute the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmat()
        if(!is.null(m)) {
            message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmat(m)
        m
}
