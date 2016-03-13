## Function for Caching Matrix and its Inverse and producing the inverse or retrieving it if already calculated

## Produces a list of functions that cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(Inverse) Inv <<- Inverse
    getInverse <- function() Inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of the matrix in the above function or if available retrieves it from the cached memory

cacheSolve <- function(x, ...) {
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$getMatrix()
    Inv <- solve(data)
    x$setInverse(Inv)
    Inv    
    ## Return a matrix that is the inverse of 'x'
}
