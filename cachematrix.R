## Calculates the inverse of a matrix and caches the result. If the inverse
## is requested again, it returns the cached value if the matrix hasn't change

## Creates a special matrix object that can cache its inverse
## To be used as an input for for cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above
## If there is a cache from a previous calculation, it returns that
cacheSolve <- function(x, ...) {
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
