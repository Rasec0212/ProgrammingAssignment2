## The functions will compute and cache the inverse of 
## a a matrix

## This first function creates a matrix which contains
## a function that will set and get the value of a matrix
## and also set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {        
        m <- NULL
        set <- function(y) {                        
                x <<- y
                m <<- NULL
        }
        get <- function() x                         
        setinverse <- function(solve) m <<- solve   
        getinverse <- function() m                  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}
## The next function calculates the inverse matrix of the matrix 
## created with "makeCacheMatrix" but it first checks if inverse 
## matrix has been calculated already, if yes, it is cached by the 
## function.
## Otherwhise, it calculates the inverse matrix and sets the value 
## of the inverse matrix in the cache via de setinverse function

cacheSolve <- function(x, ...){ 
        m <- x$getinverse()                        
        if(!is.null(m)){                               
                message("getting cached data")
                return(m)
        }
        data <- x$get()                             
        m <- solve(data, ...)                       
        x$setinverse(m)                             
        m
}
