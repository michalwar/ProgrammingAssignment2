## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
    s <- NULL                               #It is required to define the s variable
    set <- function(y) {
        x <<- y                             
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve #I set inverse matrix in order to get it leter
    getsolve <- function() s                #In this part function return inversion result
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)               #This is list is required to call specific functions set, get,...
}
##======================================================================================
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
    s <- x$getsolve()
    if(!is.null(s)) {                       #I have to cheeck wether iversion result already exist and if I return it
        message("getting cached data")
        return(s)
    }
    data <- x$get()                         #If inversion result do not rexist I will create it and return
    s <- solve(data, ...)
    x$setsolve(s)
    s
    
}
#===========================================================================