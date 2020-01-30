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

makeCacheMatrix <- function(x = matrix()) {
        ## Step 1: 
        ## "x" is initialized as a function argument
        j <- NULL
        ## Step 2:
        ## "j" i s set to NULL, initializing it as an object within the 
        ## makeCacheMatrix () environment to be used by later code in the 
        ## function.
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        ## Step 3
        ## Define the "set()" function. (Sets "x", our original matrix)
        ## assign "y" to "x" from the parent environment
        ## assign  NULL to "j" from the parent environment to clear any previous
        ## value oj "j" that may have been cached from using makeCacheMarix()
        get <- function()x
        ##Step 4
        ##Defines the get() function. Will get "x", our original matrix
        setInverse <- function(inverse) j <<- inverse
        ##Step 5
        ## Defines the setInverse() function. Will set "j" as the inverse of our
        ## original matrix
        getInverse <- function() j
        ##Step 6
        ## Defines the getInverse() function. Will get "j", our inveted matrix
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        ##Step 7
        ##Each of the previous four functions become elements in a list. They
        ## are named so that we can access them by name. CacheSolve() will 
        ## need these names from any makeCacheMatrix() type object taht it wants
        ## to operate
}


## cacheSolve is required to retrieve or calculate & store the inverse of an
## object type makeCashMatrix. It shows of us if the object has a cached inverse 
## of the original matrix, it retrieves the value if cached; if not cached.
## it calculates the inverse and caches it into the object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        ## Step 2: 
        ## Calls the getInverse() function on the object
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        ## if the value of "j" is not equal to NULL  return "j" to parent
        ## environment. (Show inverted matrix and message "getting cashed data")        
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
        ## if the value of "j" is NULL, call the get() function on object;
        ## calculates inverse; sets the inverse on the makeCacheMean object;
        ## and prints inverted matrix, returning it to parent environment.
