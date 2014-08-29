## Coursera - Introduction to R - Programming Assignment 2

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a list storing a matrix and its inverse
## that use the following functions: 
## set: stores the matrix provided by the input
## get: returns the cached matrix
## setinverse: stores the inverse matrix provided by the input
## getinverse: returns the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the stored matrix
## If the inverse was already calculated, it simply gets it from the cache
## Otherwise, its calculates the inverse, sets it, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
