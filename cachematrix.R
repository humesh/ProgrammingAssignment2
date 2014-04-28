## The following two functions deal with Matrix manipulation in R,
## especially in the context of caching matrices.
##We assume that x is a square invertible matrix

## This function takes a matrix and creates a special "matrix" object that caches
##its inverse 

makeCacheMatrix <- function(x = matrix()) {
  Mx<-NULL
  set<-function(y){
    x<<-y
    Mx<<-NULL
  }
  get<-function() x
  setInverse<-function(Invx) Mx<<-Invx
  getInverse<-function() Mx
  finallist<-list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  return(finallist)

}


## The follwoing function computes the inverse of the special matrix 
## returned by the makecacheMatrix function above. If the inverse
## has already been calculated(and the matrix has not changed) then the 
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(y) {
  ## Return a matrix that is the inverse of 'y'
  Invy<-y$getInverse()
  If(!is.null(Invy)){
    return(Invy)
  }
  else{
    NeededMatrix<-y$getInverse()
    Invy<-Solve(NeededMatrix)
    y$setInverse(Invy)
    return(NeededMatrix)
  }
}
