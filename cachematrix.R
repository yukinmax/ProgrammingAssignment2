## Provided functions cache the inverse of a matrix
## Supplied matrix should always be invertible.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinversed <- function(inversed) ix <<- inversed
    getinversed <- function() ix
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    im <- x$getinversed()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    message("inverting matrix")
    m <- x$get()
    im <- solve(m)
    x$setinversed(im)
    im
}
