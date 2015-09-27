## There are two functions in this file  - 1.makeCacheMatrix, and 2.cacheSolve, which perform Matrix Inversion operation. The first function creates a special "mayrix" object that can cache its inverse. And the second function computes the inverse of the special "matrix" returned by the first function.
#
#
#This will create a list containing a function to "set" and "get" the value of the matrix. And also to "set" and "get" the value of the "inverse" of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#
#
# This function will return inverse of the matrix. It will first check if the inverse is already computed; if so, then it will retrieve the result without computing again.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
#
#
## Sample run:
#
#> x = rbind(c(1, -1/2), c(-1/2, 1))
#> m = makeCacheMatrix(x)
#> m$get()
#     [,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0
#
##First run
#> cacheSolve(m)
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#
##Retrieving the cached data
#> cacheSolve(m)
#getting cached data.
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#
## Sample run: End
