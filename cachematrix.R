
## creates a special matrix that has functions "get", "setinverse", and "getinverse" that allows for the matrix inverse to be cached.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## It retrieves the inverse from cache if the inverse has already been calculated for a set matrix

cacheSolve <- function(x, ...) {
  
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
        
}
