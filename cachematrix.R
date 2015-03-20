## Put comments here that give an overall description of what your
## functions do
# Following the example in README, the two functions here are similary
# organized. `makeCacheMatrix` implements the object and getter and setter
# methods for caching the matrix inverse. `cacheSolve` retrives the cached
# inverse if it exists, otherwise computes it and saves in the object within
# `makeCacheMatrix` and returns the computed value too.

## Write a short comment describing this function
# This function creates a custom object that can cache the inverse of its
# matrix and return it when asked.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## Write a short comment describing this function
# This function returns the inverse of the matrix returned by 
# `makeCacheMatrix`, if the inverse has already been calculated and cached.
# Otherwise it computes the inverse calling `solver` builtin function and caches
# it within `makeCacheMatrix`and returns it also.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
