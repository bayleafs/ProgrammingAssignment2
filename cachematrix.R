## These functions a pair of functions that cache the inverse of a matrix.
## If the inverse has already been calculated, and the matrix hasn't changed, 
## then the inverse of the matrix is retrieved from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {	
	s <- NULL

	set <- function(y,...) {
		t <<- y
		s <<- NULL
	}

	get <- function() t

	setInverse <- function(solve) s <<- solve

	getInverse <- function() s

  	list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	s <-t$getInverse()

	if (!is.null(s)) {
		message("getting cached data")	
	return(s)
  	}
  
  	data <-t$get()
  	s <- solve(data)    
  	t$setInverse(s)
  
  	s
}
