## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #the first value assigned to the variable "cache" is null
  cache <- NULL
  
  #set the matrix
  set <- function(y){
    x <<- y
    cache <- NULL
  }
  #get the value of the matrix
  get <- function() x
  
  #invert the matrix with the function and get it from cache
  set_matrix<-function(inverse) cache <<- inverse
  
  get_inv <- function() cache
  
  lis(set=set, get=get, set_matrix=set_matrix, get_inv=get_inv)
  }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$get_inv()
        if(!is.null(cache)) {
                message("getting cache data")
                return(cache)}
        data<-x$get()
        cache<- solve(data,...)
        x$set_matrix(cache)
        return(cache)
}
