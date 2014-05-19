## The functions make CacheMatrix and cacheSolve allow the caching and 
## retrieval of the calculated inverse of a matrix to save computing

## makeCacheMatrix is a function of four functions needed to set up 
## a caching matrix, get the data for the original matrix, invert the 
## matrix and cache the inverted matrix
makeCacheMatrix <- function(x = matrix()) {  
        inv <- NULL
        ## set takes values for x (the intial matrix) and nulls cache
        ## matrix (inv)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get returns x (the initial matrix)
        get <- function() x
        ## setSolve creates the inverted matrix and assigns it to inv
        setSolve <- function(solve) inv <<- solve
        ## getSolve returns inv (the inverted matrix)
        getSolve <- function() inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}  

## cacheSolve returns the cached inverted matrix for the matrix of interest
## if the cache matrix (inv) is not null.  If the cach matrix is null,  it 
## calculates the inverted matrix for the matrix of interest and caches
## the result to inv and returns the cached inverted matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getSolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setSolve(inv)
        inv
}