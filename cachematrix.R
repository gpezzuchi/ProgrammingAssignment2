## The makeCacheMatrix is the analogous function to the makeVector of the readme example.

makeCacheMatrix <- function(x = matrix()) {
    # First we need a place to store the inverse value
    inverse <- NULL
    # Then we need to set the original matrix and reset the inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Now we need to get the original matrix
    get <- function() x
    # set inverse value
    set_inverse <- function(inv) inverse <<- inv
    # get inverse value
    get_inverse <- function() inverse
    
    # Returns a list of the 4 functions, this list is the special "matrix" (~ as in the special vector in the example)
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## The next function calculates the invesrse of the "special matrix" returned by 
## the makeCacheMatrix function.  
## If the inverse has already been calculated (and the matrix is unchanged 
## cachesolve retrieve the inverse from the cache.




cacheSolve <- function(x, ...) {
        #the code is analogous to the one found in the cachemean function in the example
        inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("retreiving cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}

