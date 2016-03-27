## Put comments here that give an overall description of what your
## functions do

## The following function takes a matrix as input and 
## sets up in memory cache to store a solve value and set a solve value
## into the matrix. This function doesn't not test if the matrix is a square matrix,
## but it assumes that the user inputs a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve, getSolve = getSolve)
}

## The following function checks to see if the matrix has been cached
## if so returns the value otherwise solves the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)){
      message("getting data from cache")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
