## The functions makeCacheMatrix and cacheSolve cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
 set<-function(y) {
   x<<-y
   m<<-NULL
 }
 get<-function() x
 setmatrix<-function(solve) m<<-solve
 getmatrix<-function() m
 list(set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Please wait. Getting cached data.")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

