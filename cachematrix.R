## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    #initialize where result will be stored
    minv<-NULL
    set<-function(y){
        x <<- y
        minv <<- NULL
    }
    #return the input matrix
    get<-function() x
    #set the inversed matrix
    setinvmatrix<-function(solve) minv <<- solve
    #return the inversed matrix
    getinvmatrix<-function() minv
    #return a list with results
    list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
         
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get the inversed matrix of x
  minv<-x$getinvmatrix()
  #if null, then calculate
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  #get matrix object
  m<-x$get()
  #solve m
  minv<-solve(m)
  x$setinvmatrix(minv)
  #return result
  minv
}

