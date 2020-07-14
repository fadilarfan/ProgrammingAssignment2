## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialize inverse
  
  set <-function(matrix) { #set the matrix value
    m<<-matrix 
    i <<- NULL
  }
  
  get <-function(){ #get the matrix value
    m
  }
  setInverse <-function(inverse){ #set the inverse value of matrix
    i<<-inverse
  }
  getInverse <- function(){ #get the inverse value of a matrix
    i
  }
  list(set=set,get=get,setInverse=setInverse,
       getInverse=getInverse) #list the functions of the makeCacheMatrix function
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getInverse() #get the inverse value from a makeCacheMatrix object
  
  if(!is.null(m)){
    print("Value already in cache , retrieving inverse from cache")
    m #just return the matrix if the inverse is not null (already calculated)
  }else{
    tempMat <- x$get() #if the inverse is Null then compute the inverse
    m <- solve(tempMat) %*% tempMat #By using solve to get the right-hand side of the equation and matrix multiplication
    x$setInverse(m) #set the inverse using the function in the makeCacheMatrix object
    m #return the matrix
  }
  
}
