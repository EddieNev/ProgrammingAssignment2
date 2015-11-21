## Caching the Inverse of a Matrix. 
## Functions to (1) create an object that stores a matrix and (2) cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        set <- function(y) {

## Assign y in a different environment

                x <<- y
                inv <<- NULL
        }

	  ## Return the input matrix for stage 2

        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Function computes the inverse of the "matrix" created in the previous function. 
## If inverse is already calculated, function retrieves it from cache.


cacheSolve <- function(x, ...) {

        ## Return matrix that is inverse of 'x'

 inv <- x$getInverse()
        if (!is.null(inv)) {

        ## If result present then return  “getting cached data”
                message("getting cached data")
                return(inv)
        }

	  ## If nothing in cache then get input matrix
        mat <- x$get()
	
	  ## Calculate inverse f matrix
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}




