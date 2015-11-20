## ABOUT THIS FUNCTION BY NISCHIT
##This perticular function will take the inverse of a matrix .As computation will take long time to calculate the inverse
## of metrix repeatedly . Also, in this function the matrix value is not chaning hence we preferred to cache the inverse matrix.
##and leverage it when required again rather computing it again .

## In this function we have used <<- operator which is used to assign the matrix value to an object into a object 
## that is different from current environment.Below code will create a matrix "MakeCacheMatrix" to set the value of the metrix
## , Set inverse of the martix and GET the inverse too.

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list (set = set,get = get,
              setsolve = setsolve,
              getsolve = getsolve)
        
}

## This function computes the inverse of the special "matrix"retuned by 
## makecacheMatrix above.If the inverse has laready been calculated 
## (and the matrix is already created the it will return to NULL by skipping the computation otherwise)
## it inverse the Matrix and stores the value in the cache via the Setsolve function.

cacheSolve <- function(x, ...) {
        ## Rget inverse of a metrix
        m <- x$getsolve()
        # Returns the inverse if the inverse is already calculated
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #computes inverses of a matrix if it is a new matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
y
T <- makeCacheMatrix(y)
cacheSolve(T)
