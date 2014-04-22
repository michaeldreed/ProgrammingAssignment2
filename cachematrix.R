## This R script contains two functions: makeCacheMatrix and cacheSolve
## ----------------------------------------------------------------------------
## The aim of these functions is to speed up algorithms which calculate the 
## inverse of a matrix by recording its value in a cache from which it can be 
## instantly retrieved as opposed to having to repeatedly calculate it. 

## makeCacheMatrix creates a special matrix object that can store the inverse of 
## a given matrix in its cache.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise the inverse to be null
    i <- NULL 
    
    ## This function sets the matrix 'x' to the value of a given matrix 'y'
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## This function gets the matrix 'x' and returns it
    get <- function() x
    
    ## This function sets the inverse value 'i' from a given inverse
    setinverse <- function(inverse) i <<- inverse
    
    ## This function gets the inverse value and returns it
    getinverse <- function() i
    
    ## The special matrix returned is simply a list of the above four functions
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the matrix 'x' returned by 
## makeCacheMatrix. If the inverse has previously been calculated then this 
## function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Check the x matrix's cache for the inverse value
        i <- x$getinverse()
        
        ## If there is a cache, then return the inverse value
        if(!is.null(i)) {
            message("Retrieving inverse from the cache")
            return(i)
        }
        
        ## If there isn't a cache, calculate the inverse value using the solve 
        ## function, save it to the cache and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
