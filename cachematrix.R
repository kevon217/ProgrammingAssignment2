## makeCacheMatrix takes a matrix as an argument. 
## It then creates 4 functions: set, get, setinverse, and getinverse, which are listed at the end. 
## 1) "set" allows new matrices to be initialized if wanted. it will reinitialize m as null since you will eventually have to calculate the inverse again.
## 2) "get" reproduces the matrix
## 3) "setinverse" will use the <<- operator to set m as the matrix outside the scope of the function.
## 4) "getinverse" will return the matrix stored in m if there is one.
## Listing these funtions allows the cacheSolve to access these subfunctions as indices. 

##cacheMatrix takes makeCacheMatrix as an argument and will now have access to the functions it created.
## 1) if m is a stored matrix inversion then !is.null(m) will be TRUE and the inverted matrix will be returned
## 2) if m is null, then you have to caculate the matrix inversion anew. you do this by inserting the output of get() into the solve() function
## 3) setinverse will make this value m with the <<- operator to make it accessible outside the function.
## 4) inversion results are obtained by returning m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  
}
