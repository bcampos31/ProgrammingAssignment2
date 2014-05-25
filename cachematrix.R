## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a special vector which contain is a list containign a functions to set the value of the vector,
## get the value of the vector, set the value of the inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<-inverse
        getInverse <- function() m
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Write a short comment describing this function
## This function calculate the value of the inverse of the special vector cretaed with the above function.
## First check if the inverse has been calculated. If so, it gets the inverse from the cache anf skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse 
## function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if ( ! is.null(m)) {
                print("getting cached data")
                return(m)
        }
        
        m<-solve(x$get())
        x$setInverse(m)
        m
}
