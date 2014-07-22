# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL
    
    # Get and Set functions for the 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    # Get and Set functions for the inverse
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    # Return the matrix with the appropriate functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. 
# If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    
    #    
    inv <- x$getinv()
    if (!is.null(inv)) {
        return(inv)
    }
    
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
