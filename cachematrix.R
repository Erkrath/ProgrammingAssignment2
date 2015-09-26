## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#####makeCacheMatrix: This function creates a special "matrix" (Saved) object that can cache its Inv_Matrix.

makeCacheMatrix <- function(x,Inv_Matrix) {
  Saved <- list(x,Inv_Matrix)
}


#####cacheSolve:  This function computes the Inv_Matrix of the special "matrix" returned by makeCacheMatrix above. 
#####             If the Inv_Matrix has already been calculated (and the matrix has not changed), 
#####             then the cachesolve should retrieve the Inv_Matrix from the cache

## Write a short comment describing this function
cacheSolve <- function(x) {
  #Check if the Inv_Matrix has been calculated...
  if(exists("Saved") == TRUE) {  #if so, retrieve data
    message("getting Inv_Matrix from previous calculations")
    Inv_Matrix <- Saved[2]
  }
  else { #if not, calculate the Inv_Matrix
    Inv_Matrix <- solve(x)
    message("Inv_Matrix has not been calculated previously. Calculating now...")
    Saved <- makeCacheMatrix(x,Inv_Matrix)
  }
  # Return the Inv_Matrix
  Inv_Matrix <- Saved[2]
  print(Inv_Matrix)
}
