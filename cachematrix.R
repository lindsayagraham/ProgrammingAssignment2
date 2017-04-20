## This function pair works in conjunction to either a.) compute the inverse
## of a supplied matrix, or b.) pull a cached version of the already computed
## inverse of the supplied matrix. 

## This function receives and stores the matrix to be inverted and returns a
## list of functions that can be called within the following cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        imx <- NULL
        set <- function(y) {
                x <<- y
                imx <<- NULL
        }
        get <- function() x
        setimx <- function(mtx) imx <<- mtx
        getimx <- function() imx
        list(set = set, get = get,
             setimx = setimx,
             getimx = getimx)
}


## This function receives the results of makeCacheMatrix (a list of functions
## and inherited environment variables), then computes and returns the inverse 
## matrix unless variable 'imx' is not null, indicating the inverse matrix has 
## already been calculated, and should be returned from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        imx <- x$getimx()
        if(!is.null(imx)) {
                message("getting cached data")
                return(imx)
        }
        data <- x$get()
        imx <- solve(data, ...)
        x$setimx(imx)
        imx
}
