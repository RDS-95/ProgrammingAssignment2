## These functions combines will cache the inverse of a matrix

## makeCacheMatrix creates a matrix and stores it. 
## it sets the value of the matrix, gets the value of the matrix and does the same for the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, 
		 get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the cached inverse from the function above

cacheSolve <- function(x, ...) {
       i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

