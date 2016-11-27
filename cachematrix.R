## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## function creates list, which consists of four functions:
    ## 1. set the matrix
    ## 2. get the matrix
    ## 3. set the inverse
    ## 4. get the inverse
    ## this list of functions is used as the input to cacheSolve()
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skips the computation.
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}


## m <- makeCacheMatrix(matrix(rnorm(9), nrow = 3))
## summary(m)
## Length Class  Mode    
## set    1      -none- function
## get    1      -none- function
## setinv 1      -none- function
## getinv 1      -none- function
##
## our matrix
## m$get()
## [,1]       [,2]       [,3]
## [1,] 1.46406529  1.4392234  0.1217701
## [2,] 0.07353636 -0.1236746 -0.2385649
## [3,] 1.18081277 -1.3936153  0.8864942
##
## run function the first time
## cacheSolve(m)
## [,1]      [,2]       [,3]
## [1,]  0.3873969  1.266685  0.2876647
## [2,]  0.3039646 -1.011286 -0.3139006
## [3,] -0.0381655 -3.277021  0.2514006
##
## run function the second time
## cacheSolve(m)
## getting cached data
## [,1]      [,2]       [,3]
## [1,]  0.3873969  1.266685  0.2876647
## [2,]  0.3039646 -1.011286 -0.3139006
## [3,] -0.0381655 -3.277021  0.2514006

