## This R file speeds up repeated computations of matrix inversions by caching
## the result on a particular matrix and only computing the inverse if it does 
## not yet exist in memory. Matrix inversion is performed using the Moore-
## Penrose inversion as provided in the MASS library (citation below). 
## Completed for programming assignmnet 2, 'R Programming', Coursera, 2015. 

## Citation: 
## Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. 
## Fourth Edition. Springer, New York. ISBN 0-387-95457-0 

library(MASS) ## Include the MASS library

## makeCacheMatrix <- function(x = matrix())
## This function returns a list of functions for storing and manipulating
## the inverse of a particular matrix in memory.
## Inputs: 
## 	x: A matrix. For the purposes of this assignment it is assumed square and 
## 	invertible, although squareness is not a prerequisite in this code.
## Outputs:
##	list: A list of four functions that can be used to query and set the data
##  stored in memory. 
##		get: Returns the stored matrix.
##		set: Stores the matrix x in memory and sets the inverse to null.
##		getinv: Returns the inverse of the matrix.
##		setinv: Sets the inverse of the matrix. 
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


## cacheSolve <- function(L, ...)
## This function returns the inverse of a matrix, but only performs the 
## inversion if it has not been computed previously. See the ginv function 
## from the MASS library for valid options of ...
## Inputs:
##	L: The list output of makeCacheMatrix.
## Outputs:
##	inv: The inversion of the matrix accessed through L.
cacheSolve <- function(L, ...) {
	inv <- L$getinv()
	if (!is.null(inv)){
		message('Using cached inversion.')
		return(inv)
	}
	message('Computing matrix inversion.')
	data <- L$get()
	inv <- ginv(data, ...)
	L$setinv(inv)
	return(inv) # Returning the inverse matrix.
	}
