# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cached inverse when the matrix is changed
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already calculated, retrieve it from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
