## Function 1: setting and extracting matrix/inverse from cache.
# basically contains 4 functions: setting and getting for matrix istelf and for inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m<-NULL
  
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }  # function for setting the value of matrix to alter its value
  
  get <- function() x #get value of matrix
  
  setinverse <- function(inverse) inv_m <<- inverse #set value of inverse
  getinverse <- function() inv_m #get value of inverse 
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#test the function
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) #instead of matrix, in global environment we have list of 4.
my_matrix$get()
my_matrix$getinverse() #should be NULL



## Function2: finding inverse/or fetching it from cache if saved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv_m <- x$getinverse()   #check if inverse has already been calculated and saved
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m) # if inv is not NULL, we take from cache.
  }
  data_for_inv <- x$get() #if inv is NULL, first get (extract) value of matrix 
  inv_m <- solve(data_for_inv, ...) # take inverse via function solve.
  x$setinverse(inv_m) #finally, set the inv.matrix to the value just calculated
  inv_m #return the result. 
  
}

# test the function
cacheSolve(my_matrix)
inv_m #if we simply run this will get error since it is in other environment.
#now if we run it with getinverse, we see it was saved.
my_matrix$getinverse()


