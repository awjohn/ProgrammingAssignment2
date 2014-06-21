## Create a special "matrix" object that can cache its inverse
## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
##
## see @makeVector
## usage: see http://www.mathwords.com/i/inverse_of_a_matrix.htm
##      > cells <- c(4,3,3,2)
##      > rnames <- c("R1", "R2")
##      > cnames <- c("C1", "C2")
##      > mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,dimnames=list(rnames, cnames))
##      > mymatrix
##         C1 C2
##      R1  4  3
##      R2  3  2
##      > x <- mymatrix
##      > cx <- makeCacheMatrix(x)
##      > cx$get()
##         C1 C2
##      R1  4  3
##      R2  3  2
##      > cacheSolve(cx)
##         R1 R2
##      C1 -2  3
##      C2  3 -4
##      > cacheSolve(cx)
##      getting cached data
##         R1 R2
##      C1 -2  3
##      C2  3 -4
##      >
##
makeCacheMatrix <- function(x = matrix()) {
        ## inverseMatrixCache is a variable that will
        ## store a cached copy of the matrix inverse
        inverseMatrixCache <- NULL

        ## Define a matrix mutator function
        set <- function(y) {
                x <<- y
                ## Cache invalidate: re-initialize the cache copy to start things off
                inverseMatrixCache <<- NULL
        }

        ## Define a matrix accessor function
        get <- function(){
          x
        }

        ## Define a inverse matrix mutator function
        setInverseMatrixCache <- function(inverse){
           inverseMatrixCache <<- inverse
        }

        ## Define a inverse matrix accessor function
        getInverseMatrixCache <- function(){
           inverseMatrixCache
        }

        ## Add predefined functions to matrix and return
        list(set = set, get = get,
             setInverseMatrixCache = setInverseMatrixCache,
             getInverseMatrixCache = getInverseMatrixCache)
}

## Computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the
## matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache
## see @cachemean
##
cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrixCache()

        ## Check to see if the matrix inverse has a non-nulled value
        ## and display a message...
        if(!is.null(m)) {
            message("getting cached data")
        }
        else {
            ## Otherwise
            data <- x$get()

            ## Calculate the matrix inverse with the R solve fn
            m <- solve(data, ...)

            ## Store to the cache
            x$setInverseMatrixCache(m)
        }
        m
}