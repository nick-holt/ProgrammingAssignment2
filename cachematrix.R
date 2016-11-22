## These functions allow for the inverse of a matrix to be cached rather than
## computing the inverse every time the matrix is called

## The function called makeCacheMatrix creates a special 
## matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


## Testing the functions:
##
## > set.seed(10)
## > matrix(rnorm(9), nrow = 3, ncol = 3)
## [,1]       [,2]      [,3]
## [1,]  0.01874617 -0.5991677 -1.208076
## [2,] -0.18425254  0.2945451 -0.363676
## [3,] -1.37133055  0.3897943 -1.626673
##
## > matrix <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3))
## > cacheSolve(matrix)
##            [,1]      [,2]       [,3]
## [1,]  0.6404624  2.744233 -1.0891800
## [2,] -0.3777843  3.202915 -0.4355093
## [3,] -0.6304550 -1.545961  0.1990977
## 
## Verifying that the matrix is cached upon second run:
##
## > cacheSolve(matrix)
## getting cached data
## [,1]      [,2]       [,3]
## [1,]  0.6404624  2.744233 -1.0891800
## [2,] -0.3777843  3.202915 -0.4355093
## [3,] -0.6304550 -1.545961  0.1990977
