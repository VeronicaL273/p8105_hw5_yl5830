Homework3
================
Yunjia Liu
2024-11-13

## Problem 1

Generate a birthday simulation, which generates random birthdays and
checks for duplicates.

``` r
bday_simulation = function(group_size){
  birthdays <- sample(1:365, group_size, replace = TRUE)
  return(any(duplicated(birthdays)))
}
```

Initialize the group_size

``` r
group_sizes <- 2:50
n_simulations <- 10000
probabilities <- numeric(length(group_sizes))
```

Run simulation.  
Perform simulations for the current group size using map_lgl.
mean(results) calculates the probability of at least one shared
birthday.  

``` r
for (i in length(group_sizes)) {
  group_size = group_sizes[i]
  results = map_lgl(1:n_simulations, ~bday_simulation(group_size))
  probabilities[i] = mean(results)
}
```
