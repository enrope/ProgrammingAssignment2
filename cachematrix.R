##  These functions calcualte and then cache the inverse of a matrix

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        setmt <- function(y) { 
                x <<- y
                s <<- NULL
        }
        getmt <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(setmt = setmt, getmt=getmt, setinv=setinv, getinv=getinv)
        
}

## cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$getmt()
        s <- solve(data, ...)
        x$setinv(s)
        s      
}