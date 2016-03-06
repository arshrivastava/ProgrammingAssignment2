## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  
  chdInv <- NULL ## initializing inverse
  
  ## setting x in parent environment with the provided value, remove if inverse already calculated!
  set <- function(usrVal = matrix()) {
    x <<- usrVal
    chdInv <<- NULL
  }
  
  get <- function() x
  
  ##setting up of inverse variable in parent env to required value and return the value 
  setInv <- function(invVal) {
    chdInv <<- invVal 
    return(chdInv)
  }
  
  getInv  <- function() chdInv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


##  check if there's already a cached inverse and return else find its inverse and return

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) {
        ## matrix that is the inverse of 'x'
  
  ## check if its already there
  calInv <- x$getInv() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(calInv) && is.matrix(calInv)) { 
    message("cached data ")
    return(calInv)
  }
  
  ## else the matrix
  matrxSolv <- x$get()  
  
  ## errors and warnings
  calInv <- tryCatch({ 
    solve(matrxToSolv)
  }, warning=function(w) {
    message("Invalid result")
    message(w)
  }, error=function(e) {
    message("unexpected error")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInv(calInv)
  
}
