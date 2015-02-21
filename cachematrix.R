# Create 2 functions which will reduce computing time by placing values in cache
# functions will use lexical scoping rules

# makeCacheMatrix will take a user inputed matrix and keep it in cache within 
#the function named makeCacheMatrix. Will return a list of 4 functions, set and get
#setsolve, getsolve

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


## cacheSolve is a function which will return a solved matrix inputed through
#makeCacheMatrix and keep the solved matrix in cache.

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
