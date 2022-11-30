# 9 Functions ----
# a simple function to show how it works
add_one <- function(input_data) {
  return(input_data + 1)
}
add_one(10)

number_series <- c(1,5,10)
add_one(number_series)

# 9.3 Activity 2: Write own Function ----
# Use the instructions above to complete the function below
variance <- function(input_data){
  square <- sqrt(input_data)
  ss <- sum(square)
  variance <- (ss/(length(input_data)-1))
  return(variance)
}

# Use vector (or make a new one) and try out your new function

variance(number_series)

## 9.4 Argument Defaults ----
say_hello <- function(){
  paste("Hello World")
}

say_hello()

