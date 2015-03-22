## This assignment has two functions; makeCacheMatrix and cacheSolve
## This assignment demonstrates the usage of Scoping Rules in R


# tmp1 <- matrix(c(2,3,3,2), nrow=2, ncol=2)
# mat1 <- makeCacheMatrix(tmp1) # create a list of functions for the input matrix
# mat1$get()   # returns the original matrix tmp1
# mat1$getinv() # returns NULL since inverse is set to NULL in parent env
# cacheSolve(mat1) #  checks if inverse is calculated. Since it is not, it calculates and returns inverse
# mat1$getinv() # returns the inverse calculated and cached by previous step
# cacheSolve(mat1) # checks if inverse is calculated. Since it is, it returns cached inverse

## makeCacheMatrix takes a matrix as an input and returns a list whose elemnts are functions
## on the input matrix.
## get function is used to get the input matrix from the makeCacheMatrix env.
## set function is used to set a new input matrix into makeCacheMatrix env.
## getinv function is used to get the inverse of input matrix from the makeCacheMatrix env. 
## If inverse is not already calculated, it returns NULL
## setinv function is used to set calculated inv into makeCacheMatrix env.

makeCacheMatrix <- function(data_matrix = matrix()) {
    
    stored_inv <- NULL                           
    
    set <- function(y) {
      data_matrix <<- y
      stored_inv <<- NULL
    }
    
    get <- function() {
      return (data_matrix)
    }
    
    setinv <- function(new_inv){
      stored_inv <<- new_inv
    }
    
    getinv <- function() {
      return(stored_inv)
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is used to find the inverse of the given matrix.
## cacheSolve only takes the list created using  makeCacheMatrix to find the inverse.
## If the inverse is already calculated and cached, it returns that value.
## If the inverse is null, it calculates, caches and returns the inverse. 


cacheSolve <- function(made_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  local_inv <- made_matrix$getinv()
  
  if(!is.null(local_inv)) {
    
    message("getting cached data")
    return(local_inv)
    
  } else {
    
    local_matrix <- made_matrix$get()
    local_inv <- solve(local_matrix, ...)
    made_matrix$setinv(local_inv)
    return(local_inv)
    
  }
}
