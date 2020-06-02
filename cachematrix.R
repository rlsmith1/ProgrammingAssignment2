
# These functions allow you to cache the inverse of a matrix rather than computing it repeatedly

# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#      If the inverse has already been calculated (and the matrix has not changed), 
#      then cacheSolve should retrieve the inverse from the cache.

# note: assumes that all matrices supplied are invertible

# makeCacheMatrix ---------------------------------------------------------


## Creates a "matrix" object that can cache its inverse

## Actually a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
       
       i <- NULL
       set <- function(y) {
              x <<- y
              i <<- NULL
       }
       get <- function() x
       setInverse <- function(inverse) i <<- inverse
       getInverse <- function() i
       list(set = set, 
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
       
       
}



# cacheSolve --------------------------------------------------------------



## Returns a matrix that is the inverse of "x"

# Calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
       
       i <- x$getInverse()
       if(!is.null(i)) {
              message("getting cached data")
              return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setInverse(i)
       i
       
       
}




