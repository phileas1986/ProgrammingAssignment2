## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix" object and can cache
## its inverse.
## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 

## makeCacheMatrix creates a special "matrix" object, which is a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## We suppose that the inputed matrix is invertible

makeCacheMatrix <- function(x = matrix()) {

	inv_x<- NULL
	set <- function(y){
		x <<- y
		inv_x <<- NULL
	}
	get <- function() x
	setinverse<- function(solve) inv_x<<-solve
	getinverse<- function() inv_x
	list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## The cacheSolve function calculates the inverse matrix of the special "matrix" object.
## Similarly to the cachemean function shown as example, the cacheSolve function
## checks first whether the inverse has been already calculated. If so, it will retrieve
## the value in a different environment and does not perform the inversion again.
## Otherwise, it will determine the inverse thanks to the setinverse function.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
	  inv_x<-x$getinverse()
	  if (!is.null(inv_x)){

		message("The inverse matrix has been already computed. Retrieving cached data")
		return(inv_x)  
	  }
	  data<-x$get()
	  x_inv<-solve(data,...)
	  x$setinverse(x_inv)
	  x_inv

}
