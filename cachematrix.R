# Function creates a list of 4 member functions: set, get, setInv and getInv.

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL 
  # set a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  # return the input matrix
  get <- function() x 
  # set the inversed matrix
  setInv <- function(inv) xinv <<- inv 
  # return the inversed matrix
  getInv <- function() xinv 
  # return a list that contains these functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    # return the calculated inversion
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  # return the result inwert of X
  m 
}