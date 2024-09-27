## Cache the inverse of a matrix

## This function creates a special"matrix" object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function() inv
    list(set=set,
         get=get, 
         setinverse=setinverse,
         getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve<-function(x,...){
    inv<-x$getinverse()
## Check if the inverse has already been calculated(and the matrix has not changed). 
## If yes, the cacheSolve will retrieve the inverse from the cache. 
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
## If no, the cacheSolve will do the calculation.
    data<-x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv
}

