## This code allows for creating a cached version of a matrix inverse
## Example usage:
##     testmatrix <- matrix(c(0, 2, 4, 1, 0, 2, 4, 0, 2), nrow=3, ncol=3)
##     x <- makeCacheMatrix(testmatrix)
##     cacheSolve(x)

## This function creates a special matrix object, allowing to 
## keep information on the matrix inverse in the object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns a matrix which is the inverse of x.
## It uses the R solve() function for this.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data: inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
