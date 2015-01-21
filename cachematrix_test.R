# Test using a hardcoded invertible 3x3 matrix
# This test will invert the matrix 3 times, expecting to hit the cache twice
# This test will then invert the result and compare to original - to test the inversion works
# Matrix is assumed invertible.
#
# Returns TRUE if test passes
test_3by3_matrix <- function() {
    # Load our functions under test
    source("cachematrix.R")
    
    # An invertable matrix
    x <- c(1,0,5)
    y <- c(2,1,6)
    z <- c(3,4,0)
    mat <- cbind(x, y, z)
    
    # The inverse of the above matrix, with dimnames inverted too
    x <- c(-24, 18, 5)
    y <- c(20, -15, -4)
    z <- c(-5, 4, 1)
    expected <- rbind(x, y, z)
    
    # Create the cacheable matrix for the test
    cache_m <- makeCacheMatrix(mat)
    
    # Solve this once 
    first_i <- cacheSolve(cache_m)
    # Next time around - we expect a cache hit
    second_i <- cacheSolve(cache_m)
    # Once more for luck - another cache hit
    third_i <- cacheSolve(cache_m)
    
    # Lets go backwards to test our inverse function
    cache_m_inv <- makeCacheMatrix(expected)
    exp_inv <- cacheSolve(cache_m_inv)
    
    first_equal <- FALSE
    second_equal <- FALSE
    third_equal <- FALSE
    inv_of_inv <- FALSE 
    
    expected_hits <- 2
    
    if(all.equal(first_i, expected)) {
      first_equal <- TRUE
    }
    if(all.equal(second_i, expected)) {
      second_equal <- TRUE
    }
    if(all.equal(third_i, expected)) {
      third_equal <- TRUE
    }
    if(all.equal(mat, exp_inv)) {
      inv_of_inv <- TRUE
    }
    return(first_equal&&    # We get expected result first run
           second_equal&&   # ...and second run
           third_equal&&    # ...and third run
           inv_of_inv&&     # The inverse of the inverse should be the original
          (expected_hits==cache_m$gethits()))  # The expected cache hits should be equal
}
