## This file contains functions to demonstrate caching of a matrix and its inverse in R 

## Cache a matrix and return a list of functions to perform the following -
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of matrix
## 4. Get the inverse of matrix
## The inverse of matrix is initialized to NULL when the matrix value is reset.
makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL

	set<-function(newMatrix) {
		x <<- newMatrix
		inv <<- NULL
	}

	get <-function() x

	setInverse <-function(inverse) inv<<-inverse

	getInverse <-function() inv

	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}

## Compute the inverse of a cached matrix created by "makeCacheMatrix" function.
## It first checks if the inverse has already been computed. If so, it gets
## the inverse from the cache and skips the computation.
## Otherwise, it solves the matrix inverse and sets the value of inverse in cache
## via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix,...)
	x$setInverse(inv)
	inv
}
