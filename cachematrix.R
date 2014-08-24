## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Creates a function that can cache the inverse of the created matrix
## cacheSolve - Calculates the inverse of the created matrix but if already created, will call the matrix inverse

## Write a short comment describing this function

## 1st - creates the empty matrix
## 2nd - calls the empty matrix
## 3rd - sets variables in matrix by placing the inverse of the s matrix
## 4th - calls the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## 1st - calls the inverse of matrix
## 2nd- if "s" is not empty, then print message "getting cached data" and return the inverse matrix
## 3rd - if "s" is not empty, "get" the x matrix and set it as data
## 4th - s now stores of inverse matrix
## 5th -set "s" as set inverse matrix of x and then return "s" which is the inverse matrix

cacheSolve <- function(x, ...) {
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s  
}

