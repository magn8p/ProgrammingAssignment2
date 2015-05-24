## Naga’s work
## Master - https://github.com/magn8p/ProgrammingAssignment2
## SHA-1 hash - 
## R Programming Assignment 2

## The makeCacheMatrix function creates a special "matrix" which is a list containing a fuction to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## 1. The cacheSolve function calculates the inverse of the special "matrix"
## 2. The special "matrix" which created with the makeCacheMatrix function.
## 3. However, it first checks to see if the inverse has already been calculated.
## 4. If so, it gets the inverse from the cache and skips the computation.
## 5. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
