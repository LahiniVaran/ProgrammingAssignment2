
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when setting a new matrix
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check if the inverse is already cached
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  
  # Check if the matrix is square and invertible
  if (nrow(mat) != ncol(mat)) {
    stop("Matrix must be square to compute the inverse")
  }
  
  inv <- solve(mat, ...)  # Compute the inverse
  x$setinverse(inv)       # Cache the computed inverse
  inv                     # Return the inverse
}

# Create a matrix
my_matrix <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)

# Create a cached matrix object
cached_matrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse
cacheSolve(cached_matrix)

# Retrieve the cached inverse (won't recompute)
cacheSolve(cached_matrix)

solve(my_matrix)

