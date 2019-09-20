##   Getting the matrix of the vector. Checking the existence of the vector firstly. If it is cashing, 
##   directly get its inverse. IF not, computing it and storing in the list to avoid future repeat computation.

##   Set the vector/get the vector/set the matrix/get the matrix/get the list with these variables

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get<- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##   Checking if the input vector in the list. If it is in the list, get the inverse vector directly;
##    If not, computing it and store it to the list.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}