## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  
  set <- function(y){
    x <<- y
    matrix_inv <<- NULL
  }

  get <- function() x
  setinv <- function(inv) matrix_inv <<- inv
  getinv <- function() matrix_inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$getinv()
  
  if(!is.null(matrix_inv)){
    message("getting cached data")
    return(matrix_inv)
  }
  
  data <- x$get()
  matrix_inv <- solve(data)
  x$setinv(matrix_inv)
  matrix_inv
}
