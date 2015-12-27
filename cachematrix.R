## cachematrix.R
## Programmer: dartsmith
## December 27, 2015
## Written for Assignment 2, Coursera R Programming course
##
## This program includes a pair of functions that cache the inverse of a matrix.
##     Functions: makeCacheMatrix()
##                cacheSolve()

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  
    invmat <- NULL
    setm <- function(y) {
        x <<- y
        invmat <<- NULL                   # Initialize inverse matrix
    }
    
    getm <- function() x
    setinv <- function(imatt) invmat <<- imatt
    getinv <- function() invmat
    list(setm = setm, getm = getm,
         setinv = setinv,
         getinv = getinv)   
}    


## This function computes the inverse of the special "matrix" returned 
##    by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should 
##    retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

    invmat <- x$getinv()
    if(!is.null(invmat)) {                 # Do when inverse has been found before
        message("getting cached inverse")
        return(invmat)
    }
    
    tempm <- x$getm()                 # Do when inverse has not been found before
    invmat <- solve(tempm)          
    x$setinv(invmat)
    message("not cached")
    invmat
    
}
