## Functions to cache the inverse of a matrix

## Creating a matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computing the inverse of the matrix returned above
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}