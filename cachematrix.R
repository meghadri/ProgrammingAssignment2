
### 'makeCacheMatrix' creates a closure 
###     which wraps a matrix object and its inverse, 
###     and returns a list consisting of funtions which   
###     1. get - gets the value of the matrix
###     2. set - sets the value of the matrix
###     3. getinverse - gets the value of the inverse matrix
###     4. setinverse - sets the value of the inverse matrix
###     
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    
    get <- function() x
    setInverse <- function(i) invM <<- i
    getInverse <- function() invM
    list(set = set, get = get, setinverse = setInverse, getinverse = getInverse)
}


### 'cacheSolve' is a function which takes as its argument
###     an object returned by makeCacheMatrix() 
###     as its input and returns the inverse of the
###     wrapped matrix. 
###     If the inverse is not already cached within the
###     argument object, it computes the inverse using
###     solve(x) function, and sets the inverse in 
###     the argument object.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if(!is.null(invM)) {
        message("Getting inverse from cache")
        return(invM)
    }
    
    data <- x$get()
    invM <- solve(data)
    x$setinverse(invM)
    invM
}
