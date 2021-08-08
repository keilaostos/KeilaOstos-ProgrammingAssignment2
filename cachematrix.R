## R Programming - Coursera
## Week 3 - Assignment: Caching the inverse of a Matrix
      ## Student: Keila Ostos
##
## The following functions will serve to 1) create a "matrix"
## object that can cache its inverse and 2) compute the inverse of
## said "matrix" if it has not been computed before or retrieve it
## given it has been computed before


## makeCacheMatrix() will create the matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) { #initialize x as matrix
  i <- NULL #initialize i as place to cache inverse
  set <- function(y) { # the argument y can have any other name 
                        # except x, so this way we don't have two  
                        # x's in different environments
    x <<- y 
    i <<- NULL # this line of code clears any value of i that had 
                # been cached by a prior execution of cacheSolve()
  }
  get <- function() x # gets data in matrix x
  setinvrs <- function(invrs) i <<- invrs # defines the setter for
                                          # the inverse i
  getinvrs <- function() i # gets inverse i
  list(set = set, get = get, 
       setinvrs = setinvrs, getinvrs = getinvrs
       ## listing allows us to use the $ form of the extract
       ## operator to access the functions by name rather than
       ## using the [[]] form of the extract operator, as in 
       ## aMatrix[[2]] to get the contents of the matrix aMatrix
       )
  # running makeCacheMatrix() will return a list of the different
  # functions within it. 
}


## cacheSolve will compute the inverse of the matrix defined 
## previously in makeCacheMatrix().  
    ## It CANNOT work without makeCacheMatrix

cacheSolve <- function(x, ...) {
    i <- x$getinvrs() # assigns i the value of x$getinvrs()
    if(!is.null(i)) { # if x$getinvrs() IS NOT null then 
                      # the following happens
        message("getting cached data")
        return(i) # if we've come this far, the code following this
                  # section won't run again
    }
    data <- x$get() # assigns data the value of x$get()
    i <- solve(data, ...) # solves the inverse of matrix and 
                          # assigns it to i
    x$setinvrs(i) # the inverse of the matrix i is now set,
                  # so each time you run x$getinvrs you get i
    i # returns i
}
