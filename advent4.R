library(tidyverse)


build_b_tree <- function(alternatives = c("F", "B"), size = 128){
  
  get_parts <- function(n){
    c(n %/% 2, n - (n %/% 2))
  }
  
  generate_sequences <- function(to_repeat, times){
    map2(to_repeat, times, rep) %>% 
      reduce(c)
  }
  
  iterate_until <- function(n, .f, .p){
    if(.p(n)){
      n
    } else {
      c(n, iterate_until(.f(n), .f, .p))
    } 
    
  }
    
  parts <- iterate_until(
    size
    , .f =function(x) x %/% 2
    , .p =function(p) p == 1
  )  %>% 
    head(-1) %>% 
    map(get_parts)
  
  times <- iterate_until(
    size
    , .f =function(x) x %/% 2
    , .p =function(p) p == 1)  %>% 
    tail(-1)  %>% 
    rev()
  
  map2(times, parts, function(x, y){
    replicate(x, generate_sequences(c("F", "B"), y)) %>% 
      c()  
    }) %>% 
    reduce(bind_cols) %>% 
    add_column(value = 0:(size - 1)) 
  
}

string_to_function <- function(string){
  string %>% 
    stringr::str_split("", simplify = T) %>% 
    map(.f = function(x) partial(equals, y = x ))
  }

apply_functions <- function(func_list, b_tree){
  for(i in seq_along(func_list) ){
    b_tree[ 
      example[[i]]( df_tree[ , i]) %>%  as_vector()
      , ]
  }
}


file_in <- readr::read_lines()




