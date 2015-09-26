## Put comments here that give an overall description of what your
## functions do



#####makeCacheMatrix: This function creates a list with the matrix x and its Inverse.
makeCacheMatrix <- function(x,Inv_Matrix) {
  Saved <<- list(x,Inv_Matrix) #use the operator "<<-" to assign the value to "Saved", which is out kept of the current function
}

#================================================================================================================
#####cacheSolve:  This function computes the Inv_Matrix of the special "matrix" returned by makeCacheMatrix above. 
#####             If the Inv_Matrix has already been calculated (and the matrix has not changed), 
#####             then the cachesolve should retrieve the Inv_Matrix from the cache

## CacheSolve receives x as input. But first, it checks whether the variable "Saved" already exists.
 # If "Saved" already exists, then the inverse of X is kept in its second position of the list
cacheSolve <- function(x) {
  # We need to check 2 things with the "IF" statement:
    # 1) Does "Saved" exist already??
    # 2) Are we calculating the same matrix X as before?? (Sum of the elements of the difference is 0)
  if(exists("Saved") == TRUE  && sum(x - Saved[[1]]) == 0) {  #if so, retrieve data
    message("The inverse has been already calculated. Getting Inv_Matrix from Cache")
    Inv_Matrix <- Saved[2]
  }
  else { #if not, calculate the Inv_Matrix
    Inv_Matrix <- solve(x)
    message("Inv_Matrix has not been calculated previously. Calculating now...")
  }
  # Return the Inv_Matrix
  B <- makeCacheMatrix(x,Inv_Matrix) 
  Inv_Matrix <- B[2] #return the inverse of the matrix X
}
