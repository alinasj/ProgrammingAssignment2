## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
 invmatrix <- NULL
 set <- function(y){
   x <<- y
   invmatrix <<- NULL
 }
 get <- function()x
 setinv <- function(solve) invmatrix <<- solve
 getinv <- function() invmatrix
 list(set = set, get = get,
      setinv = setinv, 
      getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinv()
  if(!is.null(invmatrix)){
    message("getting cached data")
    return (invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinv(invmatrix)
  invmatrix
}

