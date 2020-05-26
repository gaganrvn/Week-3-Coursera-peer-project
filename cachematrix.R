## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a function/object called matrix which through the makeCacheMatrix can cache the inverse of itself(matrix)
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
#below, i make the create function which creates the matrix or the inverse, based on usage of create or createInverse
create <- function(y){
  x <<-y
  inv <<- NULL  

}
#retreive function works the same as the create function, except it either retreives the matrix inverse or looks through the cache because of the if loop further down
  retreive <- function() x
  createInverse <- function(inverse) inv <<- inverse 
  retreiveInverse <- function() inv
  list(create = create,
       retreive = retreive,
       createInverse = createInverse,
       retreiveInverse = retreiveInverse)
}


#the makecachematrix create an important matrix. the function solves for its inverse, outputting the result. If the inverse was found already, the function will searc through the cache for the inverse and ouput it


cacheSolve <- function(x, ...) {
       inv <- x$retreiveInverse()
       if(!is.null(inv)) {
         message("retreiving cached inverse")
         #above, I set the inv function, along with creating an if loop that plays the message (retreiving cached inverse) when the cacheSolve function is used.
         return(inv)
       }
       mat <- x$retreive()
       inv <- solve(mat, ...)
       x$createInverse(inv)
       inv
       #with the final inv, it ends the loop, completing the function
}
