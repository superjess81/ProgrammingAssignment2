## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

## The input is a matrix ("x") and the output a list containing a function to
## set the value of the matrix ("setmatrix"), get the value of the matrix ("get"), 
## set the value of the inverted matrix ("setmatrix"), get the value of the inverted matrix ("getmatrix").

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set<-function(y){
                x<<-y
                inv_matrix<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) inv_matrix <<- solve
        getmatrix<-function() inv_matrix
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getmatrix()
        if(!is.null(inv_matrix)){
                message("retrieving cached data")
                return(inv_matrix)
        }
        matrix <- x$get()
        inv_matrix <- solve(matrix, ...)
        x$setmatrix(inv_matrix)
        inv_matrix
}