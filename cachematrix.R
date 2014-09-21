


## ------------------------------
## cacheMatrix
## ------------------------------
## desc.: 
## create an object that cache the matrix and its inverse when computed once
## makeCacheMatrix: create a matrix object that stores a matrix and its inverse
## cacheSolve(matrixObject): inverse a matrixObject. If the inverse was previously computed, skip the computation and takes it from the cache.
## ------------------------------
## example of usage:
##
## > source("cacheMatrix.R")
## > mSize<-2 # use mSize<-2000 to see impact of cacheing inverse
## > mObj<-makeCacheMatrix(matrix(runif(n=mSize*mSize,min=0,max=1),mSize,mSize)) # initialize matrix object
## > mObj$get() ## check matrix properly initialized
## > mObj$getinverse() ## check inverse is NULL
## > cacheSolve(mObj) ## first call: compute inverse and cache it
## > cacheSolve(mObj) ## next calls: retrieve invers from cache
## > mObj$getinverse()%*%mObj$get() ## check inverse properly computed. Product should be identity matrix
## ------------------------------

## makeCacheMatrix
## create an object that cache the matrix and its inverse when computed once
## methods are : 
## set(matrix) [constructor]; 
## get(); 
## setinverse(inverseMatrix) [store inverse matrix; no computation here]
## getinverse() [output inverse if previously computed, NULL if inverse not computed]
makeCacheMatrix<-function(x=matrix()){
  inverseMatrix<-NULL
  set<-function(y){
    x<<-y
    inverseMatrix<<-NULL
  }
  get<-function() x
  setinverse<-function(nInverseMatrix) inverseMatrix <<- nInverseMatrix
  getinverse<-function() inverseMatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve(matrixObject):
## compute the inverse a matrixObject. 
## If the inverse was previously computed, skip the computation and takes it from the cache.
cacheSolve<-function(x,...){
  inverseMatrix<-x$getinverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(invisible(inverseMatrix))
  }
  data<-x$get()
  inverseMatrix <-solve(data)
  x$setinverse(inverseMatrix)
  invisible(inverseMatrix)
}

