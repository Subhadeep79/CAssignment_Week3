##This function creates a special "matrix" object that can cache its inverse
makeCachematrix<- function(x= matrix()){
  inv<- NULL
  set <- function(y){
    x<<- y 
    inv<<- NULL
  } ##setting value of matrix using another function
  get <- function() {x}## getiing value of the matrix
  setinverse<- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}##setting and getting the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve<- function(x, ...){
  inv <- x$getinverse()#returns matrix thats is inverse of x and assigns to inv
  if(!is.null(inv)){
    message("getting cached data")#if the inverse is gonna retrived from cache then the mag will be displayed
  return(inv)
  }
matrix <- x$get()
inv <- solve(matrix, ...)#computing inverse by using solve function
x$setinverse(inv)#setting value of inverse in cache using setinverse function
inv
}