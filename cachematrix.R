## makeCacheMatrix stores the value of the matrix to be inverted,
## creates resets cached solutions to NULL, and creates a list
## of functions to access or modify the value of these objects

## cacheSolve accesses the current stored solution and uses it if it exists.
## If no cached solution exists, it accesses the the value of the matrix 
## to be inverted, solves for the inverse and saves the solution before
## printing it.


## This function takes the matrix to be inverted as its argument.
## The "inverse" object is set to NULL each time the matrix is redefined, as
## any stored solution would be obsolete.
## Three sub-functions are also defined and output as a list:
## get_mat returns the value of "x", which is the matrix passed to the main function
## cache_inv takes an inverse matrix and uses it to set the value of "inverse" inside
##      the main function - the <<- operator is required for this because <- would
##      modify the value of "inverse" inside the cache_inv function ONLY, and this
##      value would not be in the scope of get_inv function trying to access it.
## get_inv returns the value of "inverse" from the main function, which is the one set
##      by cache_inv. If <<- was not used in the cache_inv function this would always
##      return a value of NULL.
##
## Note that for this assignment the "set" function included in the example code is
## not strictly required, for real work it would be useful to allow resetting the values
## of the input matrix and the cached inverse without having to call the entire
## makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        
        get_mat <- function() x
        cache_inv <- function(inv) inverse <<- inv
        get_inv <- function() inverse
        
        list(get_mat = get_mat, cache_inv = cache_inv, get_inv = get_inv)
        
}


## This function takes the list created by the makeCacheMatrix function and accesses
## the subfunctions contained within that list.
## First it uses get_inv to find the current value of "inverse" within the makeCacheMatrix
## environment, and then checks if this is NULL. If the input matrix has not been redefined
## from a previous run of cacheSolve, "inverse" will be the solution instead of NULL and its
## value will be returned without having to do any further calculation.
##
## If the input matrix is new, "inverse" will be NULL and calculation of the inverse matrix
## is required. get_mat is called to set "input" to the value of the matrix to be inverted,
## and solve() used to invert it (any additional arguments passed to cacheSolve will be used
## here)
## 
## cache_inv is called to set the value of "inverse" within the makeCacheMatrix function to
## the solution, and finally the solution is printed.

cacheSolve <- function(x, ...) {
        
        inverse <- x$get_inv()
        
        if(!is.null(inverse)){
                message("Using cached matrix")
                return(inverse)
        }
        
        input <- x$get_mat()
        inverse <- solve(input, ...)
        x$cache_inv(inverse)
        
        inverse
}
