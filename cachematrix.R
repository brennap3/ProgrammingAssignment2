makeCacheMatrix <- function(passmatrix = matrix()) {
  
  
  ## set the matrix
  ## get the matrix
  ## set the inverse of the matrix
  ## get the inverse of the matrix
  
  inversematrix <- NULL
  
  # This function is the set function, what it does is it sets the matrix itself but not the inverse
  set <- function(y) {
    passmatrix <<- y 
    inversematrix <<- NULL
  }
  
  # The get function,  what it does is it gets the matrix but not the inverse
  get <- function() passmatrix
  
  #
  
  # set the inverse of the matrix using solve
  setInverse <- function(inverse) {inversematrix <<- solve(inverse)}
  
  # Get the inverse
  getInverse <- function() inversematrix
  
  # Combine into a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


cacheSolve <- function(x, ...) {
  inversematrixcache <- x$getInverse()
  if(!is.null(inversematrixcache)) {
    message("getting cached data")
    return(inversematrixcache)
  }
  mattoreturn <- x$get()
  inversematrixcache <- solve(mattoreturn, ...)
  x$setInverse(inversematrixcache)
  inversematrixcache
}

####
##test scripts
##output listed in comments
###
x=c(1,2)
y=c(3,4)
matrixxy<-as.matrix(cbind(x,y))
#      x y
# [1,] 1 3
# [2,] 2 4

foov1<-makeCacheMatrix()

foov1$set(matrixxy)

foov1$get()

#      x y
# [1,] 1 3
# [2,] 2 4

foov1$setInverse(matrixxy)
foov1$getInverse()
#[,1] [,2]
#x   -2  1.5
#y    1 -0.5

foov2<-cacheSolve(foov1)
##getting cached data 
## it does this as the data is cached
