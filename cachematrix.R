## Functions that cache the inverse of a matrix.
########## The Chinese notes use the UTF-8 ##########
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {## 参数x，默认值为matrix()
    
    ## Initialize the inverse property
    m <- NULL        ## 定义局部变量m，初始值为NULL；
    
    ## Method to set the matrix
    set <- function(y) { ## 定义一个函数set，参数y
        x <<- y      ## 调用set函数，会把参数x的值修改为y
        m <<- NULL   ## 同时把之前定义的m重置成NULL；
    }
    
    ## Method the get the matrix
    get <- function() {  ## 定义一个函数get，无参数
        x            ## 调用get函数，会返回x的值；
    }
    
    ## Method to set the inverse of the matrix
    setinv <- function(inv) { ## 定义一个函数setinv，参数inv
        m <<- inv   ## 调用setinv函数，会把m的值修改为inv
    }
    
    ## Method to get the inverse of the matrix
    getinv <- function() { ## 定义一个函数getinv，无参数
        m            ## getinv函数能返回m的值；
    }
    
    ## Return a list of the methods
    list(set = set, get = get, setinv = setinv, setinv = setinv)
    ## 返回一个list，这个list由4个函数组成
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix". above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.
##以下函数计算出了上述函数创建的特殊“矩阵”的逆矩阵。
##但是，它会首先查看是否已经计算了逆矩阵。
##如果是这种情况，那么它会从缓存中获取逆矩阵并跳过计算。
##否则，它会计算数据的逆矩阵并通过setinv函数在缓存中设置逆矩阵。

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()         ## 尝试读取缓存的逆矩阵
    
    ## Just return the inverse if its already set
    if(!is.null(m)) {            ## 如果缓存值不会空
        message("getting cached data")
        return(m)              ## 直接返回缓存值
    }
    
    ## 如果代码执行到这里，说明之前的if语句被跳过，也就说明m为空
    
    ## Get the matrix from our object
    data <- x$get()       ## 读取缓存的matrix
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data, ...) ## 求matrix的逆矩阵
    
    ## Set the inverse to the object
    x$setinv(m)           ## 把逆矩阵缓存到x的环境中
    
    ## Return the matrix
    m                       ## 返回逆矩阵
}
