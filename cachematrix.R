## This file contains functions that are used to compute and cache the inverse of a matrix
## ensuring that the computation of the inverse takes place only once

## This function is used to create the 'Matrix' object which stores the matrix and also the
##inverse after it is computed. The function can also be used to change the matrix and/or change the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL						##inv-Name of inverse matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv, getinv = getinv)

}


## This function is used to compute the inverse of a the given matrix x and is only computed when the object does not previously 
##possess an inverse. In case it does contain the inverse it warns the user that the function is receiving cached data

cacheSolve <- function(x, ...) {
inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
