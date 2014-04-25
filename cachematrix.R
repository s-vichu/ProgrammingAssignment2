## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a R matrix object and encaptulates it into a function
## consisting of it's inverse along with getters and setters for both

makeCacheMatrix <- function(x = matrix()) {
    if (is.null(x)) return(NULL)  ## If the argument is NULL, then nothing to do
    
    ## Initialize both the original and inverted matrices
    invert.matrix <- NULL
    original.matrix <- x
    
    ## Set the original matrix using "Super assignment" operator 
    setMatrix <- function(y) {
        original.matrix <<- y
        invert.matrix <<- NULL
    }
    
    ## Get the original matrix
    getMatrix <- function() {
        original.matrix
    }
    
    ## Set the inverted matrix
    setInvert <- function(invert) {
        invert.matrix <<- invert
    }
    
    ## Get the inverted matrix
    getInvert <- function() {
        invert.matrix
    }
    
    ## Return the list with all 4 operator functions
    list(set = setMatrix, 
         get = getMatrix, 
         setInvert = setInvert, 
         getInvert = getInvert)
}


## cacheSolve takes a cached matrix object and returns the inverse
## It checks to see if the invert has already been computed. If so, it confirms that the original
## matrix has not changed since last invert computation. If not changed, then it returns inverse
## from the cache. Else it computes the inverse, caches it and then returns

cacheSolve <- function(x, ...) {

    ## Get both the original and inverted matrices
    invert <- x$getInvert()    
    
    ## If the inverse had been computed before, then return from cache
    if (!is.null(invert)) {
        ## Returning from the cache
        message("Returing from cache")
        return(invert)
    } 
    ## Else compute and then return
    original <- x$get()
    invert <- solve(original)
    x$setInvert(invert)
    invert    
}
