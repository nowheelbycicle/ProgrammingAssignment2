## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL                                   ##initializes an empty matrix to store the inverse (I) in addition to the base (x)
        set <- function(y) {
                x <<- y     ##sets the base matrix to a new value
                I <<- NULL  ##makes sure that the inverse is invalidated on changing base matrix
        }
        get <- function() x         ##get lets the user see the matrix itself (x)
        setinverse <- function(inverse) I <<- inverse   ##sets the cached inverse (I) to a certain value using the operator <<-
        getinverse <- function() I  ##returns the inverse matrix (I) if called
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)  ## Return a matrix that is the inverse of 'x'
{
    I <- x$getinverse()         ## try to fill the inverse from the base matrix
    if(!is.null(I))             ## check if cache with inverse matrix is already solved / computed
    {
            message("getting cached data")  ## is cached, return cached value
            return(I)
    }
    else                       ## no cache, compute and store inverse
    {
        data <- x$get()        ## get whole matrix
        I <- solve(data, ...)  ## compute inverse
        x$setinverse(I)        ## store inverse
        I                      ## return inverse
    }

}
