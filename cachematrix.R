## This file contains two functions which implements inverse caching
## for matrices. 
## The first function creates a "special" matrix object
## that can cache its inverse. 
## The second function computes the inverse of the matrix with
## logic to return with a cached inverse if it has already been
## calculated before.

## Makes a "special" matrix (list) from the given matrix, 
## which supports getter and setter methods for both the 
## matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(newInverse) inverse <<- newInverse
	
	getInverse <- function() inverse
	
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## Gets the inverse of the matrix only if the matrix is a square matrix.
## If the inverse is already calculated, then returns with the cached
## value, else calculates the inverse, caches it and returns with the
## calculated value.
cacheSolve <- function(x, ...) {        
        cache <- x$getInverse()
        
        if (is.null(cache)) {
        	message("Inverse is not cached! Calculating inverse...")    
        	
        	matrix <- x$get()
        	
        	inverse <- tryCatch({
        		solve(matrix)
        	},
        	error = function(condition) {
        		message("Couldn't calculate inverse! Matrix must be square.")
        		NULL	
        	})
        	
        	x$setInverse(inverse)
        	
        	inverse    	
        } else {
        	message("Inverse is cached! Getting cached inverse...")
        	
        	cache
        }
}
