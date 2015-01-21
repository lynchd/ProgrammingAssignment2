## David Lynch - Assignment 2
## These functions allow the creation of a matrix instance that will cache the result of
## its inverse

# Create a function that wraps X and stores some 
# data that persists over multiple invocations of X
#
# Also expose some functions that allow retrieval and modification of that state
makeCacheMatrix <- function(x = matrix()) {
  ## Variable to store the inverse
  i <- NULL
  
  ## The number of cache hits so far (useful for testing)
  hits <- 0
  
  # Set the matrix to something new, unsetting the cached inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
    hits <<- 0
  }
  
  # Return the number of hits
  gethits <- function() hits

  # Increment the cache hit count
  inchits <- function() hits <<- hits + 1

  # Return the original matrix
  get <- function() x
  
  # Set the inverse of X
  setinv <- function(iv) i <<- iv
  
  # Get the cached inverse 
  getinv <- function() i
  
  list(set = set, get = get,
       gethits = gethits,
       inchits = inchits,
       getinv = getinv,
       setinv= setinv)
}


## Use solve to set the inverse of a matrix
## Behaviour of this function is undefined if the matrix is not invertable
## It's most likely to stop with an error
cacheSolve <- function(x, ...) {
    # Set i as the inverse - this will be null if not yet set
    i <- x$getinv()
    
    # If it is not null - we have a cache hit
    if(!is.null(i)) {
        message("Cache Hit!")
        x$inchits()  # Counting these for test purposes
        return(i)
    }
    # We need to calculate the inverse, since cache is null
    data <- x$get()     # Get the data
    i <- solve(data)    # Get the inverse
    x$setinv(i)         # Cache the inverse 
    i                   # Return the inverse
}
