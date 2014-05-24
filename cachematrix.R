## There are two functions in thsi file - makeCacheMatrix & CacheSolve. makeCacheMatrix creates a "matrix" object and CacheSolve calculates the inverse of the matrix and stores it as part of the "matrix" object created by makeCacheMatrix. 


## Creates a special "matrix" object that can store the matrix data and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can store the matrix data and its inverse.
    # 
    # Args:
    #   x: a matrix of numeric or integer type
    #
    # Returns:
    #   A special "matrix" with set, get, setinverse, getinverse functions on the "matrix" object
    
    
    getinverse = getinverse
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) {
        if(all(i== solve(x))) {
            inverse <<- i      
        } else {
            message("The given data is not the inverse of the matrix")
        }
    } 
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ##  Returns the cached inverse of the "matrix" if the inverse value has been set.  Otherwise calculates inverse of the matrix and sets its value in the matrix object.
    # 
    # Args:
    #   x: a special "matrix" object
    #
    # Returns:
    #   Returns the inverse of the matrix.
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setinverse(i)
    i
}


