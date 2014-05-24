## Matrix-Solving-Cache functions
##
## 2 functions that allow the computationally expensive solve( ) function to be cached so that it needs only be calculated once.
##
## created to fulfil Programming Assignment 2 of the Coursera/John Hopkins MOOC 
##	https://class.coursera.org/rprog-003
##
## Lucia Chen
## 14/05/2014


## Function 1: makeCacheMatrix
##
## Usage: 
##	cche_matrix <- makeCacheMatrix(x)
##	
##	inputs: 
##		x: a square matrix
##
##	outputs:
##		cche_matrix:	a list of functions that allow interaction with a copy of the original input matrix
##			$set(y):	initialises or changes the square matrix to be a copy of square matrix y
##			$get():		retrieves the matrix
##			$setinv(i):	sets/caches the inverse of the matrix to be i (i should be the result of solve() )
##			$getinv(): 	retrieves the cached inverse or returns NULL 
##
## 	example: 
##		cche_matrix <- makeCacheMatrix( matrix(rnorm(1000000),nrow=1000,ncol=1000))
##		cche_matrix$getinv() #returns NULL


makeCacheMatrix <- function(x = matrix()) {


	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	
	get<-function()x
	
	setinv<-function(inverse)inv<<-inverse
	
	getinv<-function()inv

	return(list(set=set,get=get,setinv=setinv,getinv=getinv))



}


## Function 2: cacheSolve
##
## Usage: 
##	inverse_matrix <- cacheSolve(cche_matrix)
##	
##	inputs: 
##		cche_matrix: a cachable matrix object as produced by makeCacheMatrix
##
##	outputs:
##		inverse_matrix: the inverse of the input matrix. The first time the function is called, this is computed using solve() and then stored in the cche_matrix object. On subsequent calls the stored value is retrieved.
##
## 	example: 
##
##		cche_matrix <- makeCacheMatrix( )
##		cche_matrix$set(matrix(rnorm(1000000),nrow=1000,ncol=1000))
##		
##		imatrix <- cacheSolve( cche_matrix )  #imatrix now holds the inverse of cche_matrix
##		cche_matrix$getinv() # cche_matrix also has a cached copy of imatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv<-x$getinv()

	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}

	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	
	return(inv)
}
