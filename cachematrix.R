## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list of 4 functions: set, get, setm, getm

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setm<-function(solve) m<<- solve
  getm<-function() m
  list(set=set, get=get,
       setm=setm,getm=getm)
}



## Write a short comment describing this function
## cacheSolve computes the inverse and caches the first time and then returns the cached inverse data for later time

cacheSolve <- function(x, ...) {
  inv<-x$getm()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix,...)
  x$setm(inv)
  inv
}
