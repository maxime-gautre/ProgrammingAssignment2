## There are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse. 
## We use the solve function to get the inverse.

## This function creates a list containing some functions to get 
## the value of the matrix, set the value of the matrix, get and set
## the inverse.

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
      set <- function(y) {
                x <<- y
                inv <<- NULL
      }
      
	get <- function() x
      setInverse <- function(newInv) inv <<- newInv
     	getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix with cache mechanism. 
## The first parameter is a list created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {

	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data");
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
