## this function inverse and cache a matrix
## assumption: the matrix supplied is always invertible
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        #inverse variable will store the cached inverse matrix 
        inverse<-NULL
        # defining set function for the matrix:
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        # defining get function for the matrix:
        get<-function() x
        #defining set inverse function for the matrix:
        setinverse<-function(inv) inverse<<-inv
        #defining get inverse function for the matrix:
        getinverse<-function() inverse 
        #return the list of defined functions
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function returns the inverse of the matrix.
## If inverse has already been calculated, it returns this cached inverse of the matrix
## If inverse hasn't been calculated yet, it calculates and return the inverse. 
## In addition it saves the calculated inverse in cache. 

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        # if inverse value is already calculated:
        if (!is.null(inverse)){
                message("getting cached data")
                inverse
        }
        # if inverse value wasn't calculated we're doing it now:
        data<-x$get()
        inverse<-solve(data,...)
        # cache the inverse
        x$setinverse(inverse)
        # return the inverse
        inverse
}
