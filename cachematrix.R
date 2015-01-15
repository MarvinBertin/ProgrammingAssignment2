## This R code can calculate the inverse of a matrix and store it in a cache.
## If the same matrix is passed through, the code will retreive the cache instead of recalculating the inverse

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## It first checks if the inverse is already in the cache
## if so, it skips the computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}