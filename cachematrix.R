## Neil Dunay
## Coursera R Programming
## Week 3 Programming Assignment 2
## Due Sunday, 2015-09-27
    ## Sample function caches the mean of a vector
    ## Programming Assignment function caches the inverse of a matrix.


## Example: Caching the Mean of a Vector

## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment that is different 
## from the current environment. 

## Below are two functions that are used to create a special object 
## that stores a numeric vector and cache's its mean.

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to:
    ## set the value of the vector
    ## get the value of the vector
    ## set the value of the mean
    ## get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks 
## to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and 
## sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## start with empty set
    mtx <- NULL
    ## change the matrix stored in the main function.
    set <- function(y) {
        ## assign value to other environment 
        x <<- y
        ## restore the value to null  
        mtx <<- NULL
    }
    ## return the value of the matrix x stored in the main function.
    get <- function() x
    ## store the value of the matrix into the main function 
    setinverse <- function(inverse) mtx <<- inverse
    ## return the value of the matrix
    getinverse <- function() mtx
    ## assemble 4 function matrices into a list object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## cacheSolve:
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## where x is the output of makeCacheMatrix()
    mtx <- x$getinverse()
        ## verify if the value mtx exists and is not NULL 
        if(!is.null(mtx)) {
            ## if it exists in memory, return a message and the value
            message("getting cached data")
            return(mtx)
        }
        ## otherwise calculate the inverse 
        ## store the matrix from makeCacheMatrix
        data <- x$get()
        ## calculate the inverse of the matrix  
        mtx <- solve(data, ...)
        ## store the object generated
        x$setinverse(mtx)
        mtx
}

## Note: Explanatory notation assisted with clarifying instructions:
## https://github.com/DanieleP/PA2-clarifying_instructions

## Validation at prompt:
## > A <- matrix(rnorm(16), nrow = 4, ncol = 4)
## > A1 <- makeCacheMatrix(A)
## > MtxInv1 <- cacheSolve(A1)
## > MtxInv2 <- solve(A)
## > MtxInvDiff <- (MtxInv1 - MtxInv2)
## > MtxInv1
## [,1]       [,2]       [,3]        [,4]
## [1,] -1.2132014  0.7708883 -0.1544177  0.02625515
## [2,] -0.8060708 -0.4562353 -0.1103922 -0.18907410
## [3,] -0.6754145 -0.1960254 -0.6202632  0.48241542
## [4,]  1.4406629 -0.1458714  0.5837202  0.90213859
## > MtxInv2
## [,1]       [,2]       [,3]        [,4]
## [1,] -1.2132014  0.7708883 -0.1544177  0.02625515
## [2,] -0.8060708 -0.4562353 -0.1103922 -0.18907410
## [3,] -0.6754145 -0.1960254 -0.6202632  0.48241542
## [4,]  1.4406629 -0.1458714  0.5837202  0.90213859
## > MtxInvDiff
## [,1] [,2] [,3] [,4]
## [1,]    0    0    0    0
## [2,]    0    0    0    0
## [3,]    0    0    0    0
## [4,]    0    0    0    0
## > 
