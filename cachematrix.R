## The first function, makeCacheMatrix creates a special "Matrix",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL         
  set = function(y) {
    x <<- y
    inv <<- NULL    
  }         
  get <- function() x         
  setinv = function(inverse) inv <<- inverse          
  getinv = function() inv         
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function return a matrix that is the inverse of original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  inv <-x$getinv()
  if (!is.null(inv)){ 
    message("getting cached data")                 
    inv       
  } 
  data <- x$get()         
  inv = solve(data, ...) 
  x$setinv(inv)                  
  inv

}
