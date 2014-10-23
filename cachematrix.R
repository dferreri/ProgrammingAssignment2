## The following functions create a special "matrix that is actually a list 
## and can be used to cache the inverse of a matrix

## This function creates a matrix, then sets a list equal to the output

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function either finds the inverse of a matrix, or if it has already been
## solved, reports the cached value, saving the need to recalculate. 

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
        
}
