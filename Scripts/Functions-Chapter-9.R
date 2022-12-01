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

# 9.4.1 Activity 3: Understand Arguments ----
say_hello("Phil") # doesn't work as the function was not set with any arguments

say_morning <- function(x){
  paste("Good morning", x)
}

#What about this one?
say_morning("Phil") # This one works

say_morning() #if nothing in brackets, there is no default argument set

## 9.4.1.1 Argument Defaults
say_morning_default <- function(name="you"){
  paste("Good morning", name)
}
say_morning_default()

# 9.5 Conditional Functions ----
report_p <- function(p, digits = 3){
  roundp <- round(p, digits)
  reported <- paste("p=", roundp)
  
  return(reported)
}

report_p <- function(p, digits = 3){
  reported <- if_else(p < 0.001,
                      "p < 0.001",
                      paste("p=", round(p, digits)))
  
  return_reported
}

# 9.7 Warnings and errors ----
# can make own custom/specific warnings
report_p <- function(p, digits = 3) {
  
  if (!is.numeric(p)) stop("p must be a number")
  if (p <= 0) warning("p-values cannot less 0")
  if (p >= 1) warning("p-values cannot be greater than 1")
  
  reported <- if_else(p < 0.001,
                      "p < 0.001",
                      paste("p=", round(p, digits)))
  return(reported)
}
