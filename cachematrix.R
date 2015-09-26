## Matrix inversion is usually a COSTLY computational procedure.
## makeCacheMatrix is a function that stores a LIST of FUNCTIONS. It is used
## in addition to cacheSolve function in order to avoid the  need of
## computing the inverse of a matrix that had already been computed. In case
## we want to compute a NEW matrix, then
## the code will do the whole calculation of the inverse again.
## Special attention should be paid to the (<<-) operator .

## inv<-NULL begins by setting the inverse to NULL as a position for 
## a future value
## set <- function(y){x<<-y ;inv<<-NULL} defines a function to set the vector, x,
## to a new vector, y, and resets the inverse, inv, to NULL
## get <- function() x returns the vector, x
## setinv <- function(inverse) inv <<- inverse sets the inverse, inv, to inverse
## getinv <- function() inv returns the inverse, inv
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


## cacheSolve returns the inverse of a matrix in the following 2 scenarios.
## 1-st scenario-> if the inverse had already been computed and we call the
## cachesolve function on the same assigned variable, it returns 
## the result from cache without doing the computation.
## 2-nd scenario-> if we give a new entry it does calculate its inverse
## while at the same time it stores it in the cache via the setinv function.

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
