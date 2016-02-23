## The makeCaheMatrix function takes a matrix as it argument and creates a special matrix
## where its inverse is cached by calling the cacheSolve function

## makeCacheMatrix takes a matrix (needs to be square, nonsingular) and initially sets the
## inverse of the matrix to NULL. getInverted will return the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) inverted <<- solve
  getinverted <- function() inverted
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## cacheSolve accepts a special matrix, created with makeCacheMatrix
## It first gets inverted value and returns it if not NULL
## If NULL, it gets the inverted matrix using solve function and sets that value to 
## the special matrix passed in and returns the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinverted()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data)
  x$setinverted(inverted)
  inverted
}
