
# loading crosswalk table
xwalk<- read.csv('resources/crosswalk.csv', stringsAsFactors = FALSE, header=FALSE)
xwalk.map<- as.list(xwalk$V2);    names(xwalk.map)<- xwalk$V1;    rm(xwalk)

#' Return the crosswalk hash map.
#' 
#' @return a list, essentially a hash map
#' @export
get.xwalk.map<- function() xwalk.map

#' Put categories (stemmed) into a global hash table.
#' 
#' Not exported
categories.tohash<- function(categories) {
  hash<- list()
  for (category in categories) {
    stemmed.category<- tolower(stem.all.tokens(category))
    hash[[stemmed.category]]<- append(hash[[stemmed.category]], category)
  }
  hash
}

#' Score the word.
#' 
#' @param word word to score
#' @return score
#' @export
score.word<- function(word) {
  poss<- tag.POS(word)
  capitalized<- Reduce(function(a, b) a & b, mapply(function(token) grepl('^[A-Z]', token), tokenize(word)))
  
  # scoring
  score<- 0
  # Rule 1: prefer '-ing' ending
  if ('VBG' %in% poss) score<- score+1
  # Rule 2: prefer capitalized start
  if (capitalized) score<- score+1
  # Rule 3: prefer singular
  if (('NN' %in% poss) | ('NNP' %in% poss)) score<- score+1
  
  score
}

#' Choose the best word (highest score).
#' 
#' @param words list of words
#' @return the word of the highest score
#' @export
choose.bestword<- function(words) names(which.max(mapply(score.word, words)))

#' Normalize the category.
#' 
#' @param category
#' @param all.hash hash table for all categories
#' @param xwalk.map crosswalk hash map
#' @return normalized category
#' @export
normalize.category<- function(category, all.hash, xwalk.map) {
  stemmed.category<- tolower(stem.all.tokens(category))
  if (stemmed.category %in% names(xwalk.map)) stemmed.category<- xwalk.map[[stemmed.category]]
  if (stemmed.category %in% names(all.hash)) {
    choose.bestword(all.hash[[stemmed.category]])
  } else {
    category
  }
}
