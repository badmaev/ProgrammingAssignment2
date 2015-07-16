## this function inverse and cache a matrix
## assumption: the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {

        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(inv) inverse<<-inv
        getinverse<-function() inverse 
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if (!is.null(inverse)){
                message("getting cached data")
                inverse
        }
        data<-x$get()
        inverse<-solve(data,...)
        x$setinverse(inverse)
        inverse
}
