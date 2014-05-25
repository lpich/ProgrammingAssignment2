## These functions computes and caches the inverse of a matrix 
## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly

## This function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	## This function assigns the variables in 
	## the parent function's scope acc to 
	## lexical scoping rules

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	## This function assigns variables in
	## the parent function's scope acc to
	## lexical scoping rules

	setinverse <- function(inverse) inv <<- inverse

	getinverse <- function() inv

	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)

}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	data <- x$get()

	## compute the inverse
	inverse <- solve(data, ...)

	x$setinverse(inverse)

	inverse

}
