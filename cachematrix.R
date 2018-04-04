## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, maCacheMatrix creates a special list which contains a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	invert <- NULL
	set <- function(y){
		x <<- y
		invert <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invert <<- inverse
	getinverse <- function() invert
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The following function inverts and returns the matrix created in the first function above. It first checks whether the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix, and sets the value in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinverse()
        if(!is.null(invert)){
        	message("getting cached data:")
        	return(invert)
        }
        data <- x$get()
        invert <- solve(data)
        x$setinverse(invert)
        invert
}
