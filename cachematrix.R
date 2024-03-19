makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  set <- function(y) {
    x <<- y  # Assign new matrix to x
    inv <<- NULL  # Reset cached inverse
  }
  get <- function() x  # Retrieve matrix
  setinv <- function(inverse) inv <<- inverse  # Cache inverse
  getinv <- function() inv  # Retrieve cached inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)  # Calculate inverse
  x$setinv(inv)  # Cache the inverse
  inv
}
