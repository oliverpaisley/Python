

# makeCacheMatrix() takes in a matrix and returns four functions relating
# to the matrix. The first two functions are set() and get(), which set
# and get the values of the matrix, respectively. The other two functions
# are setinverse() and getinverse(), which set and get the inverse of the matrix,
# respecitvely.

# cacheSolve() takes in the output of the makeCacheMatrix, calculates the inverse
# of the original matrix, caches that inverse, then returns it. If the inverse has
# already been cached, the function will return the cached version rather than 
# computing the inverse again.

makeCacheMatrix <- function(x = matrix()) {
  #
  # This function takes in a matrix and returns a list of functions.
  #
  # args:
  #   x: A matrix. It will be converted to a matrix if it is not one already.
  #
  # returns:
  #   set(): A function that sets the values of the matrix.
  #   get(): A function that gets the values of the matrix.
  #   setinverse(): A function that sets the values of the inverse of the matrix.
  #   getinverse(): A function that gets the values of the inverse of the matrix.
  #
  # This function will takes a matrix, and creates four functions that relate to it.
  # This list of functions is to be used on with our cacheSolve() function.
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  return(list(set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse))
}

cacheSolve <- function(x, ...) {
  #
  # This function takes in the output of the makeCacheMatrix() function
  # and returns the inverse of the original matrix. If the inverse was already
  # calculated, it becomes cached and the function will return the cached version.
  #
  # args:
  #   x: A list of functions relating to a matrix. The output of makeCacheMatrix().
  #
  # returns:
  #   The inverse of the matrix from makeCacheMatrix().
  #
  # This function first checks to see if the inverse has already been calculated.
  # If it has, and the matrix has not changed, the function will return that inverse. 
  # If not, it will calculate it, cache it, then return it.
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}