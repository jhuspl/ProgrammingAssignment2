# Function makeCacheMatrix accepts a matrix as an argument and returns a special object which is a list of 
# getter and setter functions which can be invoked to get (and set) the value of the matrix and its associated
# calculated values like it's inverse. The cacheSolve function checks if the inverse of the matrix held in the 
# special list object is cached and if so it fetches the version from the cache and returns it and if it doesn't
# exist in the object's cache, then it calculates it, sets the same in the object's cache so that it can be
# fetched next time when required and also returns the newly calculated value.




# makeCacheMatrix takes a Matrix as an argument and returns a special matrix which 
# is really a list containing functions to perform the following tasks
#
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix - in 'm'
# get the value of the inverse of the matrix - from 'm'

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL     # assign null to the value m which is the variable to hold the inverse of the matrix
  
  #***********************************************************
  # the function that sets the passed matrix to the variable
  
  setmatrix <- function(y)   
  {
    x <<- y        # variable 'x' now holds the matrix
    m <<- NULL     # variable 'm' needs to be reset as a new matrix has been passed and m needs to be recalculated
  }
  
  #************************************************************
  # getmatrix function when invoked simply returns the matrix variable 
  
  getmatrix <- function() 
  {
    return (x)
  }  
  
  #************************************************************
  # setinverse when invoked will set the value of variable 'm' whenever an inverse is newly calculated
  
  setinverse <- function(inverse) 
  {
    m <<- inverse
  }  
  
  #************************************************************
  # getinverse when invoked will return 'm' - the stored value of the inverse that has been set by the setinverse
  # function above
  
  getinverse <- function() 
  {
    return (m)
  }   
  
  #************************************************************
  # list is that special variable that holds all the information about the matrix and its inverse and if the
  # inverse is already calculated and stored in variable 'm'
  
  list( setmatrix = setmatrix, 
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse
  )  
  #************************************************************
}

############################################################################################################
#***********************************************************************************************************

# cachesolve function calculates the inverse of the special "matrix" created with the above function. However, 
# it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache  
# using the getinverse() and skips the computation. Otherwise, it calculates the inverse of the data and sets 
# the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()   # invoke getinverse() to fetch the value of the inverse held in cache
  
  if(!is.null(m))    # check that the value is not null i.e. the inverse actually exists in cache
    
  {
    message("Fetching value from cache")  # print just for understanding that the value is actually fetched from cache
    return(m)                             # return the fetched value from cache
  }
  
  #***************************************************  
  # the below code will only get executed if the value of 'm' is null i.e. the inverse does not exist in cache
  
  data <- x$getmatrix()   #invoke getmatrix function to obtain the matrix data and store in 'data' variable
  
  m <- solve(data, ...)   # store the inverse of the matrix 'data' in variable m
  
  x$setinverse(m)         # invoke setinverse() function to set the value of the inverse in cache
  
  m   #return the inverse of the matrix that was newly calculated
  
}
############################################################################################################

#                                               TESTED OUTPUT

############################################################################################################

# create a 3 x 3 matrix using random unified variables

# > testmat = matrix(runif(1:9),3,3)
# > testmat

#           [,1]       [,2]        [,3]
# [1,] 0.8274306 0.79134593 0.003530383
# [2,] 0.9315452 0.17721589 0.273063587
# [3,] 0.6818875 0.01297728 0.416809542

# Now invoke makeCacheMatrix and pass testmat as argument which will return the special matrix - store it in
# 'inputmat'

# > inputmat = makeCacheMatrix(testmat)

# Now invoke cacheSolve and pass the 'inputmat' object. This will calculate the inverse of the matrix for the 
# first time and display

#  > cacheSolve(inputmat)

#	 		       [,1]      [,2]      [,3]
#	[1,] -0.6886752  3.229752 -2.110068
#	[2,]  1.9789967 -3.353919  2.180484
#	[3,]  1.0650357 -5.179350  5.783293
#
# Now invoke the cacheSolve function again to ensure that the second time it indeed fetches the value from cache

#	> cachesolve(inputmat)
#
#	Fetching value from cache
#			       [,1]      [,2]      [,3]
#	[1,] -0.6886752  3.229752 -2.110068
#	[2,]  1.9789967 -3.353919  2.180484
#	[3,]  1.0650357 -5.179350  5.783293
#	
##################################################################################################################