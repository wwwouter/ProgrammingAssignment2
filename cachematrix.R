## Use makeCacheMatrix and cacheSolve to cache potentially time-consuming solve computations on matrices

## Creates a special matrix, which holds a matrix and can hold a cached version of it's inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y		
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse ) i<<- inverse 
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse ,
		getinverse = getinverse )
}


## Calculates the inverse of the matrix object created by makeCacheMatrix 
## It will only do the calculation when it hasn't been done before

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
