## The contents of this file are responsible for creating an object that contains
## a matrix and is capable of storing it's inverse for future use to remove the need
## for repeating the matrix inversion step with the goal of decreasing computational effort.

## Function: makeCacheMatrix
##
## This function creates a list with a set of functions that work with 
## as a matrix with a cachable inverse
##
## Parameters:
##   x - matrix: the matrix to store in the object
##
## Returns:
##   A list containing the functions required to get and set both the matrix and it's inverse
##   containing methods:
##     set(y) - sets the value of the stored matrix to y
##     get() - returns the stored matrix
##     setSolve(inverse) - stores the value of a matrix passed in the inverse parameter 
##     getSolve() - gets the value of the inverse matrix if stored, returns null if none stored

makeCacheMatrix <- function(x = matrix()) {
    ## Start off with a null inverse because it isn't needed yet
    s <- NULL
    
    ## The set function stores a new matrix
    set <- function(y) {
        x <<- y
        # a new matrix requires a new inverse so clear the stored inverse value
        s <<- NULL
    }
    
    ## The get function returns the currently stored matrix
    get <- function() x
    
    ## the setSolve function stores an inverse value
    setSolve <- function(inverse) s <<- inverse
    
    ## the getSolve function returns the value of the stored matrix inverse 
    ## or null if none has been stored
    getSolve <- function() s
    
    ## Return a list object containing the functions defined above.
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Function: cacheSolve
##
## This function checks a matrix object created by the makeCacheMatrix function
## for a stored inverse and returns it if found.  If not found it calcuates the 
## inverse, stores it and then returns it.
##
## Parameters:
##   x - list: a matrix object created by the makeCacheMatrix function to invert
##   ... - additional parameters passed to the 'solve' function
##
## Returns:
##   An inverted matrix

cacheSolve <- function(x, ...) {
    ## Look in the makeCacheMatrix list for a stored inverse
    s <- x$getSolve()
    
    ## If a cached inverse was found return it
    if(!is.null(s)) {
        ## To demonstrate that the cache worked, issue a message
        ## Remove the line below if it gets annoying
        message("getting cached data")
        
        ## Return the value of the cached inverse
        return(s)
    }
    
    ## No inverse was found
    ## Get the matrix so that we can invert it
    data <- x$get()
    
    ## Invert the matrix! (and store it in x)
    s <- solve(data, ...)
    
    ## Store the inverse matrix
    x$setSolve(s)
    
    ## Return the computed inverse matrix
    s
}
