#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve computes the value of the special matrix from the makeCacheMatrix function
# It first checks if the inverse of the matrix has already been calcuted before. If it has, !is.null(m) will be TRUE, and it will return the inverse matrix that it had calculated before.
## If the inverse matrix had not been caculated before, then it will solve the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
