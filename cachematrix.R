# makeCacheMatrix: return a list of functions to get/set values for matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL
    # Get and Set functions for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    # Get and Set functions for the inverse
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    # return the matrix with the appropriate functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. 
# If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    # return the inverse if its already set
    inv <- x$getinv()
    if (!is.null(inv)) {
        return(inv)
    }
    # return a matrix that is the inverse of 'x'
    data <- x$get()
    # calculate the inverse using matrix multiplication
    inv <- solve(data, ...)
    # set the inverse to the object
    x$setinv(inv)
    # return the matrix
    inv
}
