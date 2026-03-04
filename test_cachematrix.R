## Tests for makeCacheMatrix and cacheSolve
## Run with: source("test_cachematrix.R")

source("cachematrix.R")

## Helper: simple test runner
pass <- 0
fail <- 0

assert <- function(desc, expr) {
        result <- tryCatch(expr, error = function(e) FALSE)
        if (isTRUE(result)) {
                cat("[PASS]", desc, "\n")
                pass <<- pass + 1
        } else {
                cat("[FAIL]", desc, "\n")
                fail <<- fail + 1
        }
}

assert_error <- function(desc, expr) {
        result <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
        if (isTRUE(result)) {
                cat("[PASS]", desc, "\n")
                pass <<- pass + 1
        } else {
                cat("[FAIL]", desc, "\n")
                fail <<- fail + 1
        }
}

## ---- makeCacheMatrix tests ----

assert("makeCacheMatrix returns a list with 4 functions", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        is.list(cm) && all(c("set", "get", "setinverse", "getinverse") %in% names(cm))
})

assert("get() returns the original matrix", {
        m <- matrix(c(1, 2, 3, 4), 2, 2)
        cm <- makeCacheMatrix(m)
        identical(cm$get(), m)
})

assert("getinverse() returns NULL before inverse is computed", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        is.null(cm$getinverse())
})

assert("set() updates the matrix and clears the cache", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        cacheSolve(cm)  # populate cache
        m2 <- matrix(c(4, 3, 2, 1), 2, 2)
        cm$set(m2)
        identical(cm$get(), m2) && is.null(cm$getinverse())
})

assert_error("makeCacheMatrix rejects a non-matrix input", {
        makeCacheMatrix(c(1, 2, 3, 4))
})

assert_error("makeCacheMatrix rejects a non-square matrix", {
        makeCacheMatrix(matrix(1:6, 2, 3))
})

assert_error("set() rejects a non-square matrix", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        cm$set(matrix(1:6, 2, 3))
})

## ---- cacheSolve tests ----

assert("cacheSolve returns the correct inverse (2x2)", {
        m <- matrix(c(1, 2, 3, 4), 2, 2)
        cm <- makeCacheMatrix(m)
        inv <- cacheSolve(cm)
        isTRUE(all.equal(m %*% inv, diag(2)))
})

assert("cacheSolve returns the correct inverse (3x3)", {
        m <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), 3, 3)
        cm <- makeCacheMatrix(m)
        inv <- cacheSolve(cm)
        isTRUE(all.equal(m %*% inv, diag(3)))
})

assert("cacheSolve caches the inverse after first call", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        cacheSolve(cm)
        !is.null(cm$getinverse())
})

assert("cacheSolve returns cached result on second call", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
        inv1 <- cacheSolve(cm)
        inv2 <- cacheSolve(cm)  # should print "getting cached data"
        identical(inv1, inv2)
})

assert_error("cacheSolve raises an error for a singular matrix", {
        cm <- makeCacheMatrix(matrix(c(1, 2, 2, 4), 2, 2))
        cacheSolve(cm)
})

## ---- Summary ----
cat("\n")
cat("Results:", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
