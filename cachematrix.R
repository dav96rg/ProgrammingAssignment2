## Put comments here that give an overall description of what your
## functions do

## The function takes a matrix type object and stores it into the cache

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv_mat) m <<- inv_mat
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
  }

## Takes the matrix stored in the cache by the function makeCacheMatrix and returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  }
