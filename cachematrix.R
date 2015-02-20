## Put comments here that give an overall description of what your
## functions do

## This function creates a special object to store a matrix and to cache its 
## inverse. It creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of tne inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function (x = matrix())
{
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invX) inv <<- invX
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function calculates the inverse of the matrix created with the above 
## function. If the inverse has already been calculated, it gets the inverse
## from cache. Otherwise, it calculates the inverse and sets its value in the
## cache using the setInv function.

cacheSolve <- function(x,...){
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached inverse") 
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}