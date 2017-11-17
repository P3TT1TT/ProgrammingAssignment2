## R Programming Assignment Week 3

## makeCacheMatrix takes the matrix x as the input
## then sets the value of the matrix
## get the value of the matrix
## sets the inverse martix
## and gets the inverse of the matrix.
## CacheSolve takes the output from makeCacheMatrix
## and inputs it.
## It checks if the inverse has any value or not
## If it does it finds the inverse with the solve function
makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function() x
  setInverse<-function(Inverse)I<<-Inverse
  getInverse<-function()I
  list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
  I<-x$getInverse()
  if(!is.null(I)){
      message("Getting Cached Data")
      return(I)
  }
  data<-x$get()
  I<-solve(data,...)
  x$setInverse(I)
  I
}

## Test to check it is working.
cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)))