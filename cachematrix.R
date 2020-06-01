## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special "matrix" object that can cache its inverse, using the double arrow assignment to manage variable at different levels 

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                   x <<- y
                   inv <<- NULL
            }
            get <- function() {x}
            setinverse <- function(inverse) {inv <<-inverse}  
            getinverse <- function() {inv}
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

## Write a short comment describing this function
## this function computes the inverse of the matrix x and assigns it to inv
## if the inverse has been calculates, it can skip the computation, and returns the inverse. If not, it will do the calculation 

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
            }
            matrix <- x$get()
            inv <- solve(matrix, ...)
            x$setinverse(inv)
            inv
}
