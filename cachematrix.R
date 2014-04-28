## Put comments here that give an overall description of what your
## functions do

# Special function to make an 'object' that will hold a matrix and a cached version of its inverse
makeCacheMatrix <- function(m = matrix()){
  matrix <- m
  cached_inverse <- NULL
  
  changed <- FALSE # Invariant approach to help with performance -so that the solveCache function doesn't have to track a copy of the matrix to know whether it has changed
                   # This is fine in  this scenario as there is only one function defining what the "changed" status means.
  
  #Set the matrix that this functions wraps
  set <- function(m) {
    if(!identical(matrix, m)) changed <<- TRUE
    matrix <<- m
  }
  
  #get the matrix that this function wraps
  get <- function() matrix
  
  #setter for cached inverse
  set_cached_inverse <- function(cache){
    changed <<- FALSE
    cached_inverse <<- cache
    cached_inverse
  }
  #getter for cached inverse
  get_cached_inverse <- function(){
    cached_inverse
  } 
  
  #function to get changed status
  has_changed <- function() changed
    
  #function mapping
  list(set = set, get = get,
       set_cached_inverse = set_cached_inverse,
       get_cached_inverse = get_cached_inverse,
       has_changed = has_changed)
  
} 

# Function that takes in a CacheMatrix and if not already calculated, calculates the matrix's inverse 
# and updating the CacheMatrix to store the new value 
cacheSolve <- function(cached_matrix, ...) {
  
  ## Return a matrix that is the inverse of 'cached_matrix'
  inverse <- cached_matrix$get_cached_inverse()
  
  #Only caclulate inverse if matrix has changed or it hasn't been calculated yet
  if(cached_matrix$has_changed() | 
       is.null(cached_matrix$get_cached_inverse())){
    message("Recalculating inverse")
    #calculate and set inverse
    return(cached_matrix$set_cached_inverse( 
                              solve(cached_matrix$get())
                              ))
  
  }
  inverse
}

