###########################################################################
# In this file, two R functions, makeCacheMatrix and cacheSolve are defined.
# The former creates matrix data structure and the latter calculates
# its inverse matrix.
# As the inverse calculation is time-consuming, once the inverse is calculated,
# its value is cached until the contents of the matrix is changed.
###########################################################################

#--------------------------------------------------------------------------
# makeCacheMatrix function creates a special "matrix" which is a list
# containing a function to
# 1. set the value of the matrix (set)
# 2. get the value of the matrix (get)
# 3. set the value of the inverse (setinverse)
# 4. get the value of the inverse (getinverse)
#--------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function(inv) i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#--------------------------------------------------------------------------
# cacheSolve function calculates the inverse of the special "matrix"
# which is created with the makeCacheMatrix function.
# First, it checks if the inverse has been calculated. If so, it gets
# the inverse via getinverse function from the cache and just returns it.
# Otherwise, it calculates the inverse of the data and sets it in the cache
# via the setinverse function.
#--------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
