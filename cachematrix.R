## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creating the function
makeCacheMatrix <- function(q= matrix()){
  inverse <- NULL
  ##setting the matrix
  set <- function(z){
    q <<- z
    inverse <<- NULL
  }
  ##getting the matrix
  get <- function(){
    q
  }
  ##setting the inverse of the matrix
  setInverse <- function(inv){
    inverse <<- inv
  }
  ##getting the inverse of the matrix
  getInverse <- function(){
    inverse
  }
  ##returning the list of everything aforementioned, set & getting inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function


cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  q <- x$getInverse()
  ## if already set, return matrix
  if(!is.null(q)){
    return(q)
  }
  ## getting matrix
  mat <- x$get()
  ##calculating inverse
  q <- solve(mat) %*% mat
  ##set inverse
  x$setInverse(q)
  ##return
  q
}

##testing
testMat <- makeCacheMatrix(matrix(1:4, 2, 2))
testMat$get()
testMat$getInverse()
cacheSolve(testMat)
