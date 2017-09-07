## Put comments here that give an overall description of what your
## functions do

#Both functions below cache the inverse of a matrix to reduce unncessary additional computations and instead
#call upon the cached value, if available. 

## Write a short comment describing this function

#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix_2) inverse <<- matrix_2
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
        
}


## Write a short comment describing this function

#The cacheSolve function computes the inverse of the special "matrix" returned in makeCacheMatrix.
#If the inverse has already been calculated then cacheSolve will retreive the inverse from the cache

cacheSolve <- function(x, ...){
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
        ## Return a matrix that is the inverse of 'x'

