# Create 2 functions which will reduce computing time by placing values in cache.
# functions will use lexical scoping rules to cache the data.

# makeCacheMatrix will take a user inputed matrix and keep it in cache within 
#the makeCacheMatrix function. The function will return a list of 4 functions, set, get
# setsolve and getsolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# The following function solves the above matrix 
# created with the above function. However, it first checks to see if the
# matrix has already been solved. If so, it gets the solved matrix from the
# cache and skips the computation. Otherwise, it solves the matrix 
# and sets the value of the matrix the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
