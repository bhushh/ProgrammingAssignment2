## Put comments here that give an overall description of what your functions do
## As matrix inversion is a costly computation, the below code caches the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix - Creates a matrix object that can cache its inverse

makeCacheMatrix<-function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function()x
        setInverse<-function(inverse) inv<<-inverse
        getInverse<-function() inv
        list(set=set,get=get, 
        setInverse=setInverse, 
        getInverse=getInverse)
}
##cacheSolve - computes the inverse of the above matrix and retrieves it from the cache

cacheSolve<-function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("Inversed Matrix is as follows")
                return(inv)
        }
        inter<-x$get()
        inv<-solve(inter,...)
        x$setInverse(inv)
        inv
}
