## Put comments here that give an overall description of what your
## functions do

## This function gets the matrix ,sets its value, sets its inverse value and gets it inverse value.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<-inverse
  getinverse <-function() inv
  list(set = set,get= get,
       setinverse=setinverse,getinverse=getinverse)
  
}


## This function gets the checks for inverse matrix in the cache or else it finds the inverse value for the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
    
  }
  mat_data <-x$get()
  inv <-solve(mat_data,...)
  x$setinverse(inv)
  inv
  
  }
