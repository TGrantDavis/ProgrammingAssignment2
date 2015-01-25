### A pair of functions that cache the inverse of a matrix, 
###    saving the compute time for repeated calculations of an inverse.

### The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
### The input 'x' is a matrix, assumed to be square and invertable.
### The return is a list of 4 functions which set/get the value of the input matrix,
###    and set/get the value of the inverse matrix.
### The inverse is not calculated in this function, just stored.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


### The 'cacheSolve' function computes the inverse of the special "matrix" created in makeCacheMatrix.
### If the inverse has already been calculated (and the matrix has not changed), 
###    then the cachesolve should retrieve the inverse from the cache.
### The 'makeCacheMatrix' function must be run on the matrix first.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    message("calculating and caching inverse")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}