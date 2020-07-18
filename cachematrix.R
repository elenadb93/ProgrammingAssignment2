## The makeCacheMatrix function is able to create a specific matrix object that can cache
## its inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  
  m_inv <- NULL
  
  ## getmx returns the matrix 
  getmx <- function(){
    
    return(x)
    
  }
  
  ## setmx enables to reset the initial matrix 
  setmx <- function(y = matrix()){
    
    x <<- y
    
    m_inv <<- NULL
    
  }
  
  ## compinv enables to compute the inverse of the matrix 
  compinv <- function(){
    
    m_inv <<- solve(x)
    
  }
  
  ## getinv returns the inverse of the matrix 
  getinv <- function(){
    
    return(m_inv)
    
  }
  
  list(getmx = getmx, 
       setmx = setmx,
       compinv = compinv, 
       getinv = getinv)

}


## The cacheSolve function computes the inverse of the matrix returned by the makeCacheMatrix
## function. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  m_inv <- x$getinv()
  
  if(!is.null(m_inv)){
    
    message('getting cached data')
    
    return(m_inv)
    
  }
  
  m_inv <- solve(x$getmx())
  
  return(m_inv)  
  
}
