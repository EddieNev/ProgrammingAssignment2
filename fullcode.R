setwd("c:/Users/samantha/ProgrammingAssignment2")

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## Caching the Inverse of a Matrix. 
## Two functions used to (1) create a special object that stores a matrix and 
##(2) caches its inverse.
 
## This function creates a special "matrix" object that can cache its inverse.
 
 makeCacheMatrix <- function(x = matrix()) {

### set the value of inv to NULL
   inv <- NULL  
         set <- function(y) { 
                 x <<- y	## caches the inputted matrix.CacheSolve checks for changes
                 inv <<- NULL  ##set the value of inv to NULL
         }
         get <- function() x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
 
 }
 
 
## This function computes the inverse of the special "matrix" created in the previous function. 
## If the inverse has already been calculatedthe function should retrieve it from the cache.
 
 
 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
         if (!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
         inv
 }
 my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
 my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
 my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

