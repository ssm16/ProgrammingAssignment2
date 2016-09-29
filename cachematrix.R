## The functions in this file can be used to calculate the inverse of a matrix.
## If the inverse was already calculated, the result will be taken from the
## cache. In order to do so, a special matrix object is necessary which already
## brings some functionality with it (see makeCacheMatrix).

## makeCacheMatrix is a function to create a matrix object including functions
## - to set the content of the matrix (set)
## - to get the content of a matrix (get)
## - to set the inverse of the matrix (setInverse)
## - to get the inverse of the matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        } 
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse,
             getInverse=getInverse)               
}


## This function calculates and returns the inverse of a matrix. If the inverse
## was already calculated, it returns the inverse from cache.

cacheSolve <- function(x, ...) {
        ## get inverse from cache if already there
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("Taking inverse from cache.")
        } else {
                # cache was empty, calculate inverse and store it in cache
                data <- x$get()
                inv <- solve(data,...)
                x$setInverse(inv)
        }        
        inv
}
