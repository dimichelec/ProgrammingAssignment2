
## These two functions can be used to create a special object that
## accepts an invertible matrix and which it stores and cache's its
## inverse after calculating it the first time it is requested.
## Afterwards, it returns the cache'd version.


## This function creates the special list object. The result
## from invoking it is a list object of 4 functions used to
## get or set the matrix data of the object and its inverse.
##
## Only the get() and set() functions should be used directly
## on the object. The getInverse() and setInverse() functions
## are used by the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}


## This function returns the inverse of matrix data of the
## special list object passed into it. The first time it
## is called on an object, it will calculate the inverse
## and cache the result. Afterwards, it will only return
## what it had previously cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
