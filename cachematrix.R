## Create a matrix wrapper that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # cached inverse
  
  # Set a new matrix and clear cached inverse
  set <- function(y) {
    if (!is.matrix(y)) stop("Input must be a matrix.")
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Cache an inverse
  setinverse <- function(value) inv <<- value
  
  # Get the cached inverse (if any)
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute (or retrieve cached) inverse of the special matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  if (!is.matrix(mat)) stop("The stored object is not a matrix.")
  if (nrow(mat) != ncol(mat)) stop("Matrix must be square to invert.")
  
  # Compute inverse; propagate a clear error if singular
  inv <- tryCatch(
    solve(mat, ...),
    error = function(e) stop("Matrix is singular or not invertible: ", e$message)
  )
  
  x$setinverse(inv)
  inv
}