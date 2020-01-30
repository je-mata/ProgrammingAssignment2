## makeCacheMatrix() and cacheSolve() work in tandem. makeCachemMatrix creates
## an object consisting of a stored matrix and it's inverse, as well as four 
## functions used to calculate them and store them. cacheSolve shows of us if 
## the object has a cached inverse of the original matrix, it retrieves the 
## value if cached; if not cached it calculates the inverse and caches it into 
## the object. 

## makeCacheMatrix() stores a matrix and it's inverse ("x" and "j"). It creates
## an object that contains the two values and four functions. These four
## functions - get(), set(), setInverse() and getInverse() -
## are returned to the parent environment as a list upong running

##NOTE: more detailed commentary of both functions on "cachematrix_extra_comments.R"
##file in this repository


makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}


## cacheSolve is required to retrieve or calculate & store the inverse of an
## object type makeCashMatrix. It shows of us if the object has a cached inverse 
## of the original matrix, it retrieves the value if cached; if not cached.
## it calculates the inverse and caches it into the object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}

##NOTE: more detailed commentary of both functions on "cachematrix_extra_comments.R"
##file in this repository
