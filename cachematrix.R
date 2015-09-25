## Matrix inversion is usually a COSTLY computational procedure.
## makeCacheMatrix is a function that stores a LIST of FUNCTIONS. It is used
## in addition to cacheSolve function in order to avoid the  need of
## computing the inverse of a matrix that had already been computed. In case
## we want to compute a NEW matrix, then
## the code will do the whole calculation of the inverse again.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
