## Coursera Abraham Uribe 
## R Programming
## Programming Assignment 2  Week 3
## For qualified by fellow tasks Lexical Scoping 
## Goal : Creating two functions: makeCacheMatrix and cacheSolve 
##
## Helping me with the explanation of https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping and foro 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Example of use
##
## After compiling both functions (below) makeCacheMatrix and cacheSolve in rstudio run:
##
## c <- matrix(c(2,5,1,3), nrow=2, ncol=2)    ## set matrix 2 x 2
## c    ## see matrix   
## t <- makeCacheMatrix(c)  ## set t cache matrix
## cacheSolve(t)    ## see inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m		
}

