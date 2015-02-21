## This is an R function that is able to cache potetially time consuming 
## computations of matrix inversion

## The following function creates a special "matrix" object
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" object 
## returned by the makeCacheMatrix function above
## If the inverse has already been calculated then the following function 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
