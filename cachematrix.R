

#Please note I have chosen to use my own parameter names rather than x, y, etc - as a better coding practice

## Write a short comment describing this function
##Create a function makeCacheMatrix that receives a martix (to invert and store in cache)
#makeCacheMatrix creates the object so we extract the value of the cached inverted matrix. 
#This object is an argument to the cacheSolve function

makeCacheMatrix <- function(matrixInit=matrix()){
  #initialize the inverted matrix to NULL
  matrixInv = NULL
  
  #set function takes an argument, to set the matrix, this is to put in a "new" matrix
  #as an example, lets assume new matrix is M2. If a new object is created x <-makeCacheMatrix(M1), then "set" can be used 
  # on x as x$set(M2) to then be able to invert M2 with cacheSolve. 

  set <- function(matrixInput){
    matrixInit <<- matrixInput
    matrixInv <<- NULL
  }
  
  
  #setter for the new invertedMatrix, setter receives the calculated inverse matrix and sets the value
  #getter for the calculated invertedMatrix
  
  get <- function() matrixInit
  setInverse <- function(invertedMatrix) matrixInv <<- invertedMatrix
  getInverse <- function() matrixInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#cacheSolve calculates the inverse of the matrix and sets the value
#IF the inverse already exists, obtains it from the cache (from the cache object created)
#Please note I have chosen to use my own parameter names rather than x, y, etc - as a better coding practice


cacheSolve <- function(mInit,...){
  
  #check if cache exists where mInit=object of type makeCacheMatrix
  #if cache does not exist, calculate the inverse of the matrix
  #after calculating, set the inverse of the matrix in cache object
 
  matrixInv <- mInit$getInverse()
  if(!is.null(matrixInv)){
    #inverse may have been calculated, so get it
    print("getting from cache")
    return(matrixInv)
  }
  data <- mInit$get()
  matrixInv <- solve(data, ...)
  mInit$setInverse(matrixInv)
  matrixInv
}
