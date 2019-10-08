set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000,size=n, replace = TRUE),
  v=runif(n=n, 0, 10000)
 # w=c(1,2,5,6,7,9) ,      #  small dataset for test
 # v=c(1,6,18,22,28,36)
)
# x,y,z for test
x <- knapsack_objects[1:4,]
y <- knapsack_objects[1:8,]
z <- knapsack_objects[1:12,]
# enumerate all different combinations using a binary representation of 1 to 2^n
# put all different combinations  in a matrix "combination"
# store all value=1 in each line of  the matrix to vector a by command which()
# calculate the corresponding weights and values in data.frame x using sum(x[a,1]) and sum(x[a,2])
# store the maximum value when weight is less than W into a list "result"
brute_force_knapsack <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(W>0)
  n <- nrow(x)
  combination <- matrix(nrow = 2^n,  ncol = n)
  # combination[,] <- 0
  storebin <- vector(length = n)
  for (i in 1:2^n) {
    binary <- intToBits(i)
       for (j in 1:n) {
         storebin[j] <- as.numeric(binary[j])
       }
      combination[i,] <- storebin
   }
   best_value <- 0
   result <- list(value=0, element=0)
   for (i in 1:2^n) {
       a <- which(combination[i,]==1)
       sum_weight <- sum(x[a,1]) 
       sum_value <- sum(x[a,2])
       #cat(sum_weight,sum_value, "/n  ")   # test the process
       #print(a)
       if(sum_weight<=W & sum_value>best_value) {
         best_value <- sum_value
         best_items <- a
         actual_weight <- sum_weight
         result <- list(value=best_value,elements=best_items, weight=actual_weight)
         }
   }
   
    return(result)
    
}
    
    