## makeCacheMatrix is similar to the makeVector sample function,
## It will set, get, setInv ( set the inverse value), getInv ( get the Inverse value)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        res <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list ( set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve is similar to cachemean sample function,
##it will calculate the inverse value if you haven't already have one
##or get the cache value if you already have.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ... )
    x$setInv(inv)
    inv
}
