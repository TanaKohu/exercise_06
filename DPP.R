library(dplyr)
library(gtools)

double_digestion_problem <- function(xa, xb, xab) {
  arrangement_of_fragments_a <- permutations(length(xa), length(xa), xa)
  arrangement_of_fragments_b <- permutations(length(xb), length(xb), xb)
  
  
  for (a in 1:nrow(arrangement_of_fragments_a)) 
  { arr_frag_a <- arrangement_of_fragments_a[a, 1:length(xa)]
    
    
    for (b in 1:nrow(arrangement_of_fragments_b))
    {
      
      arr_frag_b <- arrangement_of_fragments_b[b, 1:length(xb)]
      
      
      position_map_a <- c(0,cumsum(arr_frag_a))
      position_map_b <- c(0,cumsum(arr_frag_b))
      position_map <- c(position_map_a, position_map_b)
      combined_positions <- unique(sort(position_map))
      
      differences <- sort(diff(combined_positions))
      
      if (length(differences) == length(xab))
      { 
        condition <- differences == xab
        if (all(condition))
        {
          print('Riesenie')
          print(cumsum(arr_frag_a)[1:length(cumsum(arr_frag_a))-1])
          print(cumsum(arr_frag_b)[1:length(cumsum(arr_frag_b))-1])
          
        } else
          
        {
          next
        }
        
      } else {
        next
      }
      
      
      
      
      
    }
  }
  
}


xa <-  c(2,3,5,10)
xb <- c(3,7,10)
xab <- c(1,2,2,5,5,5)
double_digestion_problem(xa, xb, xab)
