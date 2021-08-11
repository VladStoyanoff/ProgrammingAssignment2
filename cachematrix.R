## The following are a pair of functions that cache the inverse of a matrix.

## -------------------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL                                         #NULL represents null objects. We assign it to our varible z that will represent our invertible matrix
        set <- function(y) {                              #We set the value of the matrix(using a function)
                x <<- y                                   #The double arrow opperator (<<-) allows us to modify variables in the parent levels. 
                z <<- NULL                                #The single arrow operator allows us to set values only in the environment we are operating in
                
        }
        get <- function() x                               #get the value of the matrix
        setInverse <- function(inverse) (z <<- inverse)   #set the value of the inverse
        getInverse <- function() (z)                      #get the value of the inverse
        list(set = set, get = get,                        #the list allows us to call the values
             setInverse = setInverse,
             getInverse = getInverse)
}

## -------------------------------------------------------------------------

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
        z <- x$getInverse()                               #returns a matrix that is inverse of x and assigns it to our variable z
        if(!is.null(z)) {                                 #this loop allows us to call the inverse of z if it's already been calculated before
                message("getting cached data")
                return(z)
        }
        mat <- x$get()                                    #get our matrix and assign it to a variable named mat
        z <- solve(mat, ...)                              #to compute the inverse of a matrix solve() is the standart R function
        x$setInverse(z)                                   #set the value of the inverse
        z                                                 #call the variable z, that's is supposed to be the inverse of our matrix
}

## -------------------------------------------------------------------------
