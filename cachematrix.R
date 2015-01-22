## cachematrix.R creates a pair of functions that can hold the value 
## of a matrix in cache memory and compute the inverse of a matrix if it has not yet
## been solved for and cached.  In order to do this the program checks to see 
## if the inverse has been cached before computing it.

## The following function creates a special "matrix", which is really a matrix containing
## a function to 1. Set the value of the matrix, 2. Get the value of the matrix, 
## 3. Set the value of the inverse, 4. Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x <<-y
    m<<-NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function(m)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## This function computes the inverse of the special matrix formed with makeCacheMatrix
## If the inverse has already been calculated and the value has not changed, then cacheSolve
## should return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse
      if(!is.null(m))  {
           message("getting cached data")
           return(m)
      }
      data <- x$get()
      m <- solve(matrix,...)
      x$setInverse(m)
      m
  }

