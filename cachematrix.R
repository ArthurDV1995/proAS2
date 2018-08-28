## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  set <- function(y){
    x <<- y
    x_inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) x_inv <<- inverse
  getInv <- function() x_inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInv()
  x_mat <- x$get()
  
  #checks that inverse exists and that matrix hasn't changed
  if(!is.null(x_inv) && x_mat == x) {
    message("getting cached inverse")
    return(x_inv)
  }
  #otherwise, finds inverse
  x_inv <- solve(x_mat, ...)
  x$setInv(x_inv)
  x_inv
  
}    
