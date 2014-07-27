## The following pair of functions can compute and cache the inverse of a
## matrix.


## This function create a special matrix object that stores a matrix and can
## cache its inverse
## It offers the following functions:
## set - to set the value of the matrix
## get - to get the value of the matrix
## setinverse - to set the value of the inverse matrix
## getinverse - to get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        # the inverse gets stored here
        inverse <- NULL
        
        # sets the matrix (and (re)sets the inverse to NULL)
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # returns the matrix
        get <- function() x
        
        # sets the inverse
        setinverse <- function(inv) inverse <<- inv
        
        # returns the inverse
        getinverse <- function() inverse
        
        # list of available functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function returns a matrix that is the inverse of 'x'
## where 'x' is returned by the makeCacheMatrix function above.
## It first checks if the inverse has already been computed, if yes, it
## returns the cached data. If no, it computes the inverse, caches it and
## returns it.
cacheSolve <- function(x, ...) {
        
        # try to get the inverse from the cache
        inverse <- x$getinverse()
        
        # if the inverse is set, print a message and return the inverse
        if (!is.null(inverse)) {
                message("getting cached data")
                return (inverse)
        }
        
        # if the inverse is NULL (i.e. not yet computed) ...
        # retrieve the matrix from x ...
        data <- x$get()
        # compute the inverse
        inverse <- solve(data)
        # and cache the inverse in x
        x$setinverse(inverse)
        
        # return the inverse
        inverse
}
