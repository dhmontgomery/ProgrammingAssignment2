## These two functions will find the inverse of a matrix and cache it in memory. If the inverse of a particular matrix is already cached, it simply returns that cached inverted matrix rather than recalculating it. If the inverse has not been cached, then it calculates it.
## These functions assume that the supplied matrix is invertible.

## makeCacheMatrix provides a list of functions that cacheSolve can use to either return or calculate and cache the inverse of a matrix. It also resets i, which stores the inverted matrix, to NULL to ensure proper results when a matrix is solved.

## Thanks to DanieleP for insight into how to understand the processes at work in these functions.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list (set = set, get = get, 
			setinverse = setinverse,
			getinverse = getinverse) 
}


## cacheSolve uses the "get", "getinverse" and "setinverse" functions from makeCacheMatrix to first check if the inverse of a matrix x has been calculated, to return that cached value if it has a non-null value, and otherwise to recall the matrix, invert it, cache the value and return it.

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