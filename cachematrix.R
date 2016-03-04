## These functiona allow for the results of relatively time consuming computations to be cached. This allows the result to be called 
##upon when needed rather than be recalculated

##The first function, makeCacheMatrix creates a special "matrix", 
##which is really a list containing a function to

##set the value of the matrix - this matrix is cached
##get the value of the matrix 
##set the value of the inverse matrix - this matrix is cached
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                
                x <<- y
                i <<- NULL
        
        }
        
        get <- function() x
        setmatrix <- function(solve) i <<- solve
        getmatrix <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


##The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if 
##the inverse matrix has already been calculated. If so, it gets the 
##inverse matrix from the cache and skips the computation. Otherwise, 
##it calculates the inverse matrix of the data and sets the value of 
##the inverse matrix in the cache via the setmatrix function.


cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setmatrix(i)
        i
}
