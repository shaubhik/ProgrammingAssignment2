## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

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
