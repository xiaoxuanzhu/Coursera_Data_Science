## R Programming: Programming Assignment 2. 
## This script contains two functions.
## The first one will set a Matrix and the second one can cache its inverse.

## The first function is named as "makeCacheMatrix". 
## In this function, a matrix will be set and 4 functions will be defined.
## This function will return a list that contains the 4 functions.
## Variable "i" will hold the inverse of the matrix if there is one.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}


## The second function is named as "cacheSolve".
## This function takes the returned list of "makeCacheMatrix" as input.
## It will first check whether variable "i" is NULL.
## If "i" is NULL, this function will take the matrix from "makeCacheMatrix" and calculate its inverse.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
