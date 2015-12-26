## This R file speeds up repeated computations of matrix inversions by caching
## the result on a particular matrix and only computing the inverse if it does 
## not yet exist in memory. Matrix inversion is performed using the Moore-
## Penrose inversion as provided in the MASS library (citation below). 
## Completed for programming assignmnet 2, 'R Programming', Coursera, 2015. 

## Citation: 
## Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. 
## Fourth Edition. Springer, New York. ISBN 0-387-95457-0 

## This function returns a list of functions for the storing and manipulating
## the inverse of a particular matrix in memory.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){ ## If we set the matrix, clear out the inverse.
		x <<- y 					## This prevents previous values from lingering.
		inv <<- NULL
	}
	get <- function() x ## Return the original matrix.
	getinv <- function() inv ## Get the inverse.
	setinv <- function(inverse) inv <<- inverse ## Set the inverse. Note the scope
	list( 
		set = set,
		get = get,
		getinv = getinv,
		setinv = setinv
		) ## Return a list of functions
}


## This function returns the inverse of a matrix, but only performs the 
## inversion if it has not been computed previously. See the ginv function 
## from the MASS library for valid options of ...

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)){
		message('Using cached inversion.')
		return(inv)
	}
	message('Computing matrix inversion.')
	data <- x$get()
	inv <- ginv(data, ...)
	x$setinv(inv)
	return(inv) # Returning the inverse matrix.
	}
