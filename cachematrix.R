# The function makeCacheMatrix creates a special "matrix" with a list containing a function to 
## set the value of matrix
## get the value of matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  matrix <- NULL
  
  # set function to set the value of the matrix
  set <- function(newmatrix) {
    x <<- newmatrix
    matrix<<- NULL
  }
  
  # get function to get the matrix
  get <- function() x
  
  # setmatrix function to set the value of inverse matrix
  setmatrix <- function(inverse) matrix <<- inverse
  
  # get matrix function to get the value of inverse matrix
  getmatrix <- function() matrix
  
  # list of functions
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


# cacheSolve function returns the inverse of the matrix
## it calculates the inverse only when the inverse of the matrix is not cached.
## if the inverser of the matrix is available it will return it without calculating it again.

cacheSolve <- function(x, ...) {
  
  # Check if the matrix is in cache
  m <- x$getmatrix()
  if(!is.null(m)) {
    
    message("Getting the inverse matrix from cache")
    
    # Return the inverse matrix from cache
    return(m)
  }
  
  #It calculates the inverse matrix of the data and sets the value of
  #the inverse matrix in the cache via the setmatrix function.
  
  message("Calculating the inverse of the matrix")
  mat <-x$get()
  m <- solve(mat)
  x$setmatrix(m)
  
  # Return the inverse matrix calculated
  m
}
