## Вычисление обратной матрицы может быть довольно затратным.
## Для того, чтобы не выполнять вычисления повторно, лучше сохранить полученный результат.

## Данная функция создает объект, который хранит матрицу, обратную исходной.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Данная функция находит матрицу, обратную той, которая была создана функцией makeCacheMatrix()
## Если обратная матрица уже посчитана (и матрица не изменилась), функция должна возвращать обратную матрицу из кэша

cacheSolve <- function(x, ...) {
        ## Получение матрицы, обратной данной
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
