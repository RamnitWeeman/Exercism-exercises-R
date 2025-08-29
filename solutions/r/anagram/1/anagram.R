anagram <- function(subject, candidates) {
  
  subject_split <- sort(strsplit(tolower(subject), "")[[1]])
  
  candidates_split <- lapply(candidates, function(x) sort(strsplit(tolower(x), "")[[1]]))
  
  i <- 1
  anagrams <- c()
  non_anagrams <- c()
  
  while (i <= length(candidates_split)) {
    cand <- candidates_split[[i]]  
    if (length(cand) == length(subject_split)) { 
      if (identical(cand, subject_split)) {
        if (tolower(candidates[i]) != tolower(subject)) {
          anagrams <- append(anagrams, candidates[i])   
        } else {
          non_anagrams <- append(non_anagrams, candidates[i])
        }
      }
    }
    i <- i + 1
  }
  
  return(anagrams)
}

anagram("stone", c("tones","bones","Stone","onest"))