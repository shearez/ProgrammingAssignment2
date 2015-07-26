## The pair of functions below will compute the inverse of a square (e.g. 2x2, 3,3, etc.) matrix = x



makeCacheMatrix <- function(x = matrix()) {  ## The function collects the variable 'x' and declares the functions required for the 'cacheSolve' function to work.
  
  m <- NULL              
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



cacheSolve <- function(x, ...) {   ## This function checks if the value 'm" has been calculated already. If not, it calculates the value and stores the information into 'm'.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
