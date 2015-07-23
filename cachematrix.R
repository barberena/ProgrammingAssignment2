## Cache the inverse of a matrix since they can take a long time to compute
##
## How to use ---
## First, load in this source:  
##   source("cachematrix.R")
##
## Let's say you have a "square" matrix called matx, so pass matx to
## makeCacheMatrix saving the return value, which is the special matrix.
##	specialMatx = = makeCacheMatrix(matx)
##
## Now we can call cacheSolve providing our special Matrix, which returns
## the cached matrix inverse.
##	cacheSolve(specialMatx)
##


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
