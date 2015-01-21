## This function creates a special "matrix" object that can cache its inverse

##makeCacheMatrix creates a special “matrix”, which is really a list containing a function to ◦set the value of the matrix
## ◦get the value of the matrix
## ◦set the inverse of the matrix
## ◦get the inverse of the matrix


makeVector <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function cacheSolve calculates the inverse of the special “matrix” created with the 
## above function it first checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the 
##inverse of the data and sets the inverse matrix in the cache via the setinverse function.
cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}

