## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'        
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
