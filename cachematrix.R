## makeCacheMatrix() creates a list containing function to create and set 
## matrix and inverse of the matrix

## cacheSolve () checks if the inverse of the matrix is already there otherwise
## calculats the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y)
  {
    x<<-y
    inv<<- NULL  #stores matrix in cache
    
  }
  get<- function()x #retrieve matrix
  setinv<- function(solve) inv<<- solve #set inverse
  getinv<- function() inv #get inverse 
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv() 
  if(!is.null(inv)){   #check if there is a matrix inverse
    message("geting cached data")
    return(inv)
    
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinv(inv)
  inv
}
