## Put comments here that give an overall description of what your function do 

## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## The following function Caches the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) 
        {
                
                # 1. to initilize and set the value of the matrix
                m_inv <- NULL
                set <- function(y) 
                        {
                                x <<- y
                                m_inv <<- NULL
                        }
                
                # 2. to get the value of the matrix
                get <- function() 
                {
                        x
                }
                
                # 3. set the value of inverse of the matrix
                setinverse <- function(inv) 
                {
                        m_inv <<- inv
                }
                # 3. get the value of inverse of the matrix
                getinverse <- function() 
                {
                        m_inv
                }
                
                list(set=set, get=get, 
                setinverse=setinverse, getinverse=getinverse)

        }


## Write a short comment describing this function


# The followin function computes the inverse of the matrix returned by makeCacheMatrix(). 
# if the matrix inverse has already been calculated, in which case it will return the values from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        m_inv <- x$getinverse()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        y <- x$get()
        m_inv <- solve(y)
        x$setinverse(m_inv)
        m_inv
}
