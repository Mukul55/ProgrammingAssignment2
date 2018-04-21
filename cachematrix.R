## These functions written in partial fulfillment of Coursera Data Science: 
## R Programming Week 3 Assignment: Caching the Inverse of a Matrix

## This function Function makeCacheMatrix gets a matrix as an input, 
## set the value of the matrix, get the value of the matrix, set the 
## inverse Matrix and get the inverse Matrix. The matrix object can 
## cache its own object. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(m){
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## The function cacheSolve takes the output of the previous matrix 
## makeCacheMatrix(matrix) as an input and checks inverse matrix from
## makeCacheMatrix(matrix) has any value in it or not.In case inverse
## matrix from makeCacheMatrix((matrix) is empty, it gets the original
## matrix data from and set the invertible  matrix by using the solve
## function.In case inverse matrix from makeCacheMatrix((matrix) has
## some value in it , it returns a message  "Getting cached data" and
## the cached inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInv()
        if (!is.null(inverse)){
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInv(inverse)
        inverse
}

TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$get()
CacheMatrix$getInv()

cacheSolve(CacheMatrix)

