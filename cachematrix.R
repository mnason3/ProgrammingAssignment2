##The two functions below creates a special "matrix" object
## that stores a matrix and caches its inverse

## the makeCacheMatrix function creates a special matrix object
## this object holds a list containing functions to
## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the value of the Inverse
## 4)get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<-inverse}
    getInverse <- function() {inv}
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the 
## special matrix created in the above function.
## Note: It first checks to see if the inverse is already been calculated


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cashed data")
        return(inv)
                
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv

}
