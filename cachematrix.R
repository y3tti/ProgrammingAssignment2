## Function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 	## by default cached object is NULL
        cachedResult <- NULL
	## replace original matrix and drops previous cached result
        set <- function(y) {
                x <<- y
                cachedResult <<- NULL
        }

	## retrieve original matrix
        get <- function() x

	## store inverse result to the cache
        setSolve <- function(invertedMatrix) cachedResult <<- invertedMatrix

        ## retrieve the inverse from the cache
        getSolve <- function() cachedResult

        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get original matrix
        data <- x$get()
	## calculates invers of the matrix
        m <- solve(data, ...)
	## store inverted matrix to cache
        x$setSolve(m)
        m
}
