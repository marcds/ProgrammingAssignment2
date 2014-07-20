## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


## The function makeCacheMatrix creates a special "matrix" object that 
# can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
# set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
#get the value of the matrix
        get <- function() x

#use the solve function to find the inverse of a matrix and cache it using a free floating variable
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated, and the matrix has not changed, 
# then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
