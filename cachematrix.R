## Two functions are provided to facilitate caching of matrix inverse calculations
## makeCacheMatrix creates the cacheMatrix object
## cacheSolve returns the inverse of the cacheMatrix (cached if already exists)

## makeCacheMatrix returns a cacheMatrix object which is a list containing 4 functions
## set(x) sets the object a matrix value x and removes the cached inverse if it already exists
## get() returns the matrix
## setinv(cinv) sets the calculated inverse
## getinv() returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(cinv) inv <<- cinv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a cacheMatrix object. If an inverse
## has already been caculated and caached then this is returned. Else
## a new inverse is calculated and cached

cacheSolve <- function(x, ...) {
    ## get existing cached inverse
    inv <- x$getinv()
    
    ## if inverse already exists return it
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## calculate inverse, cache it and return it
    dat <- x$get()
    inv <- solve(dat,...)
    x$setinv(inv)
    inv
}
