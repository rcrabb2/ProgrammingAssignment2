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
   ##if This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

  ##This checks to see if the inverse has been already calculated
  inv <- x$getInv()
  
  ##If the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##This solves for the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
