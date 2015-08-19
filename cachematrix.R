## Solution code for Coursera R Programming Course
## Week 3 - Assignment: Caching the Inverse of a Matrix


## The code contains two functions - makeCacheMatrix and cacheSolve -
## and illustrates how the values of an inverse matrix can be cached
## so that when it is needed again (and the matrix has not been changed),
## it can be looked up in the cache rather than be recomputed.

## makeCacheMatrix is assigned to an object holding four functions
## output is a list containing the following four functions:
        ## 1: set: changes/sets the matrix stored
        ## 2: get: reads/returns the matrix stored
        ## 3: setinv : changes/sets the inverse matrix stored (in the cache 'invm')
        ## 4: getinv : reads/returns the interse matrix stored (in the cache 'invm')

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x    <<- y
                invM <<- NULL
        }
        get <- function() x
        setmean <- function(mean) invM <<- mean
        getmean <- function() invM
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}        
        

## cacheSolve verifies if an inverse matrix is already in memory in invm
## if this is the case then the existing inverse matrix as well as "getting cached data" is displayed
## if an inverse matrix does not exist it is calculated and stored in the object assigned with cacheSolve
## and the inverse matrix is displayed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getmean()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setmean(invM)
        invM #Inverted Matrix
}


## TEST

  ## Simple 2x2 matrix

  M <- matrix(c(0,2,1,0),2,2)        ## Creates a simple 2 x 2 matrix
  M #Original Matrix
  testCachedM <- makeCacheMatrix(M)  ## Generate the makeCacheMatrix object
        
  cacheSolve(testCachedM)            ## Calculates the inverse matrix or retrieves the existing inverse using the cacheSolve function
        
  cacheSolve(testCachedM)            ## Running the cacheSolve again without changing the matriX will show the cached inverse matrix
        

  # Random 3x3 non-singular matrix
  M <- matrix(runif(9,1,100),3,3)     ## Creates a 3x3 non-singular matrix
  M #Original Matrix
  testCachedM <- makeCacheMatrix(M)  ## Generate the makeCacheMatrix object
        
  cacheSolve(testCachedM)            ## Calculates the inverse matrix or retrieves the existing inverse using the cacheSolve function
        
  cacheSolve(testCachedM)            ## Running the cacheSolve again without changing the matriX will show the cached inverse matrix
        





        