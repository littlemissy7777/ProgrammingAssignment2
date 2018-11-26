## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  setinv <- NULL ##set the inverse as null
  
  ##set matrix value
  set <- function (y)
  { x <<- y
    setinv <<- NULL}
  ##get value of matrix argument
  get <- function() x
  ##set value of matrix (inverse)
  setinverse <- function(inverse) setinv <<- inverse
  ##get the inverse of the matrix value      
  getinverse <- function() setinv
  ##list the attributes of the matrix      
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  setinv <- x$getinverse()
  ## if statement if inverse is not null; getting cached data will be printed       
  if(!is.null(setinv)) {
    message("getting cached data")
  ## return the set matrix        
    return(setinv)
  }
  ##solving of inverse of the matrix      
  data <- x$get()
  setinv <- solve(data, ...)
  x$setinverse(setinv)
  ##print calculated inverse of the matrix      
  setinv
}

