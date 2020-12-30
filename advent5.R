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
    replicate(x, generate_sequences(alternatives, y)) %>% 
      c()  
    }) %>% 
    reduce(bind_cols) %>% 
    add_column(value = 0:(size - 1)) 
  
}


equals <- function(x, y){
  x == y 
}


string_to_function <- function(string){
  string %>% 
    stringr::str_split("", simplify = T) %>% 
    map(.f = function(x) partial(equals, y = x ))
  }

apply_functions <- function(func_list, b_tree){
  for(i in seq_along(func_list) ){
    b_tree <- b_tree[ 
      func_list[[i]]( b_tree[ , i]) %>%  as_vector()
      , ]
  }
  b_tree$value
}

file_in <- readr::read_lines("advent/input5.txt")

fb_tree <- build_b_tree()
rl_tree <- build_b_tree(alternatives = c( "L", "R"), size = 8)

first_part <- stringr::str_sub(file_in, 1, 7)
second_part <- stringr::str_sub(file_in, 8, 10)
  
f_b_examples <- map(first_part, string_to_function)
f_b_applied <- map_int(f_b_examples, apply_functions, b_tree = fb_tree)
  
r_l_examples <- map(second_part, string_to_function)
r_l_applied <- map_int(r_l_examples, apply_functions, b_tree = rl_tree)
  
final<- tibble(
  front_back = f_b_applied
  , r_l = r_l_applied
  , seat_id  = (f_b_applied * 8) + r_l_applied
)

final <- final %>% 
  arrange(desc(seat_id)) %>% 
  mutate(
    diff = seat_id - lag(seat_id)
  ) %>% 
  filter(diff == min(diff, na.rm = TRUE))


final





