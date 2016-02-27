## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse

## The makeCacheMatrix function creates a special "matrix", which exposes the following functions:
## 1. set - sets the value of the matrix
## 2. get - returns the value of matrix
## 3. setinverse - sets the value of the matrix inverse
## 4. getinverse - returns the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above
## function. It first checks to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Othersise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}