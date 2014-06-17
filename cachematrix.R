## ASSIGNMENT 2 IN R PROGRAMMING

## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and
## their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

################################################

# This function creates a special "matrix" object
# that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        
        finalValue <- NULL # make bin
        
        set <- function(y){
                x <<- y # assign new value
                transM <<- NULL # initialize our target
        }
        
        get <- function() return(x) # print value

        transMatrix <- function(transValue) finalValue <<- transValue
        
        getMatrix <- function() return(finalValue)
        
        list(set=set, get=get, 
             transMatrix=transMatrix, 
             getMatrix=getMatrix)
}


#################################################

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        finalValue <- x$getMatrix()
        
        if (!is.null(finalValue)){
                message("getting cached data")
                return(finalValue)
        }
        
        data <- x$get()
        transValue <- solve(data, ...)
        x$transMatrix(transValue)
        return(transValue)
}


##################################################

# test

## assume that the matrix supplied is always invertible.

testFnc <- makeCacheMatrix()
matValue <- matrix(c(1,4,0,2),2,2)
testFnc$set(matValue)
testFnc$get()
cacheSolve(testFnc)
cacheSolve(testFnc)


