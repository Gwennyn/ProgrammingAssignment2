## These functions will return the inverse of a given
## matrix (assuming that the matrix is indeed inversible).
## If the matrix already exists and hasn't been changed,
## a cached version of the matrix will be returned.

## This function stores the inverse of a matrix,
## so it can be retrieved if it's asked for again.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	setinverse = setinverse, getinverse = getinverse)
}


## This function checks whether the inverse of the matrix
## has been calculated before. If so, it returns the cached
## version of it, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
