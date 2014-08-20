## These functions provide a way to cache a matrix and to compute and cache the
## inverse of the matrix (if it has one).   

## Function makeCacheMatrix creates a "matrix" object that can cache two values, 
##   a matrix and the inverse of the matrix (if it has one). The matrix can be supplied 
##   as an argument  to the function or assigned later. The function returns a list of 
##   four functions that can be used to access (get and set) the two stored values.  

makeCacheMatrix <- function(x = matrix()) { 

	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	
	setinv <- function(inv) xinv <<- inv
	getinv <- function() xinv
	
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Function cacheSolve is intended to be used in conjunction with 
##   function makeCacheVector. The argument in a cacheSolve call must 
##   be an object x created by makeCacheMatrix. If that object represents
##   an invertible matrix, this function will return the inverse of 
##   that matrix. It will compute the inverse and store it in the 
##   makeCacheMatrix object x if the inverse has not previously been
##   stored there. Otherwise, it simply fetches the inverse from x. 

cacheSolve <- function(x, ...) {

	## Argument 'x' must be an object created by makeCacheMatrix. 
	## Return is a matrix that is the inverse of 'x'
	## Side effect: If the inverse is computed, it is stored (cached) in 'x'.

	inv <- x$getinv()
	if(!is.null(inv)) {
		## use cached value
		message("using cached inverse")
		return(inv)
	}
	## get cached matrix
	mtrx <- x$get()
	## compute inverse
	inv <- solve(mtrx, ...)
	## cache the inverse
	x$setinv(inv)

	inv
}

