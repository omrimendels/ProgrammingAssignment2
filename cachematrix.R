## This program allows the caching of the calculation of a matrix inverse. Instead of re-calculating the inverse,
# one can use the cached value in case it was already calculated.

#makeCacheMatrix returns a list of functions. Each function returns:
# get: returns the original matrix
# set: sets the original matrix
# setinverse: sets the inverse of the input matrix
# getinverse: gets the inverse of the input matrix if it was already calculated. otherwise it returns NULL.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve is a method that holds a matrix of type makeCacheMatrix. This function returns the inverse
# of the original matrix in case it was already computed, or computes the inverse in case it was not computed yet.
# The first inverse calculation is kept in cache for later use.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


##TESTING
v1<-c(0,0,0,0,1)
v2<-c(1,0,0,0,1)
v3<-c(1,1,0,0,1)
v4<-c(1,1,1,0,1)
v5<-c(1,1,1,1,1)
mat<-rbind(v1,v2,v3,v4,v5)
class(mat) #should be "matrix"

#create the special matrix:
special.mat<-makeCacheMatrix(mat)
#1. retrieve original matrix
mat2<-special.mat$get()
mat==mat2

#2. set new matrix
mat[5,5]<-5
special.mat$set(mat)
special.mat2<-special.mat$get()
mat==special.mat2

#3. calculate inverse for the first time
