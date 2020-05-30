## Put comments here that give an overall description of what your
## functions do

## This function is use to create the four functions that will 
## set, get, bothe matrix and the associated inversed matrix in cache


makeCacheMatrix <- function(x = matrix()) {
  
  ## i is representing the inversed matrix
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function checks first if the matrix has already been calculated
## If yes, the value in cache is returned
## If no, the inversed matrix is computed an stored in cache for the next time.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## i is representing the inversed matrix
  
  ## Call the function to retrieve the value in cache  
  i <- x$getinv()
  
  ## If the value is found, it is returned as a result
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If the functions continues, it is because the no value was found
  ## First the matrix is read from the cache
  data <- x$get()
  ## Then the inversed matrix is computed
  i <- solve(data, ...)
  ## And finally stored in the cache
  x$setinv(i)
  ## inversed matrix is finally returned
  i
}