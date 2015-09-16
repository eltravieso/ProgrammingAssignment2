## This function takes a square matrix and provides the inverse values
## it also caches the values
## if the values of the matrix provided where processed in the 
## previous run of the function then it provides the cahced values

## Takes the values provided and caches them into one list
## Calculates the Inverse and stores into another list

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## takes the provided matrix, checks if values the same as previous
## run, if yes then return chaced data, if no the recalculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    
    if(!is.null(m)) {
        
        message("getting cached data")
        return(m)
        
    }
    
    data <- x$get()
    
    m <- solve(data)
    
    x$setInverse(m)
    
    m

}
