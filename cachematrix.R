##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

##The objective of this assignment is to write a pair of functions that assists in caching the inverse of
##a matrix. The purpose of caching the inverse is to return the computed inverse matrix again when required
##as apposed to recomputing it.

##The function makeCacheMatrix creates a special matrix which is a list containing a function to 
##carry out the following steps:
##1.Set the value of the matrix
##2.Get the value of the matrix
##3.Set the matrix inverse
##4. Get the matrix inverse

##Each step is indicated in the function created below;

##makeCacheMatrix function creates the special matrix used in the cachSolve function
makeCacheMatrix <- function(x = matrix()) {  
  
  I <- NULL                                 ##Value initiated to NuLL
  set <- function(y) {                      ##Set the value of the matrix
    x <<- y
    I <<- NULL
  }
  get <- function() x                       ##Get the value of the matrix
  setI <- function(inverse) I <<- inverse   ##Set the matrix inverse
  getI <- function() I                      ##Get the matrix inverse
  list(set = set, get = get,                ##Summarise result in the form of a list
       setI = setI, 
       getI = getI)
}

##The cacheSolve function below is used to compute the matrix inverse and cache the result.
##The purpose of caching the matrix inverse is to the return it in the future when required. Below we apply the if
##statement to determine if the inverse has been cached, if yes, the message "getting cached data" is displayed and
##the funtion returns the inverse.
##If inverse has not been cached, the inverse is then obtained, cached and returned.
cacheSolve <- function(x, ...) {
  I <- x$getI()                             ##Get the value of the matrix
  if(!is.null(I)) {                         ##If the inverse has been computed, return the inverse
    message("getting cached data")
    return(I)
  }
  data <- x$get()                           ##If the inverse has not been computed, get the matrix
  I <- solve(data, ...)                     ##Apply the solve function to find the matrix inverse
  x$setI(I)                                 ##Cache the resulting inverse
  return(I)                                 ##Return the inverse
}

