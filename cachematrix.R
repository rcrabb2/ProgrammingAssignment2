## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This sets the cache and inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##Seting the  cache
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  
  ##Getting the cache
  get <- function() x
  
  ##Setting the inverse
  setInv <- function(inverse) inv <<- inverse
  
  ##Getting the inverse
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
