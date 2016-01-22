## The following two functions create a matrix object that caches
## its inverse. The cacheSolve function computes the inverse of
## the matrix returned by makeCacheMatrix. If the inverse has
## already been calculated, and the matrix has not changed,
## cacheSolve will retrieve the inverse from the cache. It also
## prints that it is retrieving the cached data.

## The first line creates makeCacheMatrix as a function of x with
## the default of x being a matrix. testmat is set to null.
## set and get define and retrieve the values of the matrices 
## in specific places defined outside the current environment 
## using <<-. setmat and getmat set and get the value of the
## inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  testmat<-NULL
  set<-function(y){
  x<<-y
  testmat<<-NULL
}
get<-function()x
setmat<-function(solve)testmat<<-solve
getmat<-function()testmat
list(set=set,get=get,setmat=setmat,getmat=getmat)
}


## cacheSolve first takes the results of makeCacheMatrix and 
## tries to determine whether or not the value has been 
## calculated before by testing for the variable testmat to be
## not null. If it is not a null value, it retrieves the cached
## information. If testmat is a null value, that means we have
## not calculated the inverse before, so we retrieve the matrix
## and compute the inverse, storing it as testmat for future
## cache retrieval.

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  testmat<-x$getmat()
  if(!is.null(testmat)){
    message("getting cached data")
    return(testmat)
  }
  data<-x$get()
  testmat<-solve(data,...)
  x$setmat(testmat)
  testmat
        
}
