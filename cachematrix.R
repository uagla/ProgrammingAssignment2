#### This script is divided in two sections/functions: 
#### Section A: makeCacheMatrix : This function creates a special matrix object that can cache its inverse.
#### Section B: cacheSolve:  Returns the inverse of a matrix A created with the above mentioned 
#### function (makeCacheMatrix).

###############################################################################################
########################################## Section A ##########################################
###############################################################################################

## makeCacheMatrix:  A matrix cache maker
## Parameter: X matrix
## Output: A list of functions for manipulation
##

makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    
    
    set <- function(y) {
         x <<- y
        inverseX <<- NULL
    }

    get <- function() x
    setInverse<- function(inverse) inverseX <<-inverse
    getInverse <- function() inverseX
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
 
###############################################################################################
########################################## Section B ##########################################
###############################################################################################


## cacheSolve:  It returns the inverse matrix, with the solve() function. 
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
## Parameter: X matrix. X must be not null. 
## Output: The inverse matrix. 
##


cacheSolve <- function(x, ...) {

             ## Return a matrix that is the inverse of 'x'

                inverseX <- x$getInverse()
                if (!is.null(inverseX)) {
                        message("getting cached matrix")
                        return(inverseX)
                } else {
       
             ## setting the cache 
             data<-x$get()
             inverseX <- solve(data,...)
             x$setInverse(inverseX)
             return(inverseX)
             }
}