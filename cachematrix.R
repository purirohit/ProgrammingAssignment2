#Author : Rohit Puri
#Purpose:R programming asssigment 2

## Whenever called this function sets the inverse to null  
##Other functions inside it are accesed only when cacheSolve is called 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
#get returns the original matrix
    get <- function() x
#setmatrix stores the inverse of matrix for later use 
    setmatrix <- function(inverse) m <<- inverse
#getinverse is called by cacheSolve to retrieve the saved object
    getinverse <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getinverse = getinverse)
}


## when called the fuction first checks if the inverse already exists 
##for the matrix. If the inverse is not available it will call the 
##functions in makeCacheMatrix to create inverse and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
