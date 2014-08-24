## For large matrices calculating the inverse can an expensive process, both in 
## time and computational power. For this reason it can be useful to cache the
## result after calculating it the first time, to save both time and processing
## power for subsequent inverse checking (given that the matrix has not changed).

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions to 
## 1. set the value of the matrix (and clear the cache)
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # New matrix added
    m <<- NULL # cache is for old matrix, clear it
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()               # Get data from the cache
  if(!is.null(m)) {                 # Check to see if the cache has been filled
    message("getting cached data")
    return(m)                       # If the inverse has been cached we return it and are done
  }
  data <- x$get()       # Get the matrix
  m <- solve(data, ...) # calculate the inverse
  x$setinverse(m)       # Store the result in cache
  m
}
