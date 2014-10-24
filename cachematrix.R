# Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( myMatrix = matrix() ) { 
          
     Inverse <- NULL 
        # Method to set the matrix 
            setMatrix <- function( matrix ) { 
                             myMatrix <<- matrix 
                             Inverse <<- NULL 
                     } 

        
   # Method the get the matrix 
     getMatrix <- function() { 
  	# Return the matrix 
     	myMatrix 
     } 
 
        
     # Method to set the inverse of the matrix 
     setInverse <- function(inverse) { 
         Inverse <<- inverse 
     } 
 
       
     # Method to get the inverse of the matrix 
     getInverse <- function() { 
         # Return the inverse property 
         Inverse 
} 
 
        
    # Return a list of the methods 
     list(setMatrix = setMatrix, getMatrix = getMatrix, 
          setInverse = setInverse, 
          getInverse = getInverse) 
 } 
 

 

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.
cacheSolve <- function(x, ...) { 
 
        
    # Get a matrix that is inverse of 'x' 
    myMatrix <- x$getInverse() 


   #Return inverse if its already exists 
   if( !is.null(myMatrix) ) { 
     message("getting cached data") 
     return(myMatrix) 
    } 

        
   #Get the matrix
   data <- x$getMatrix() 
        
   #Calculate the inverse using matrix multiplication 
   myMatrix <- solve(data) %*% data 


   #Set the inverse to the object 
   x$setInverse(myMatrix) 

        
   #Return the matrix 
   myMatrix 
 } 

## Unit Test
##source("c:\\Coursera\\R_Programming\\programmingassignment2\\cachematrix.r")
##> funMatrix <- makeCacheMatrix()
##> summary(funMatrix)
# Length Class  Mode    
# setMatrix  1      -none- function
# getMatrix  1      -none- function
# setInverse 1      -none- function
# getInverse 1      -none- function
# > funMatrix$setMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
# > funMatrix$getMatrix()
# > cacheSolve(funMatrix)
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# >cacheSolve(funMatrix)
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
