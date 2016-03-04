## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse1 <- NULL
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse1 <<- inverse
  getinverse <- function() inverse1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inverse1=x$getinverse()
  if(!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
  }else{
    message("inverse1 is null")
  }
    
  data <- x$get()
  #Solve Function gets the inverse
  inverse1 <- solve(data)
  x$setinverse(inverse1)
  inverse1
}
