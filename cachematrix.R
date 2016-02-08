## This function stores/retrieves
## a Matrix and its inverse in/from the cache

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function (mtrx2){
        x <<- mtrx2
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function calculates the inverse of a Matrix if it is not calculated already
##otherwise returns the value from the cache 

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting inverse from cache")
        return (inv)
    }
    mtrxc <- x$get()
    inv <- solve(mtrxc, ...)
    x$setinv(inv)
    inv
}
