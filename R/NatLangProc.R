
# natural language processing
# reference: https://stackoverflow.com/questions/30995232/how-to-use-opennlp-to-get-pos-tags-in-r

#' tokenizer
#' @importFrom openNLP Maxent_Word_Token_Annotator
word_token_annotator<- Maxent_Word_Token_Annotator()
#' Return the singleton of maxent word token annotator
#' 
#' @return maxent word token annotator
#' @export
get.token.annotator<- function() word_token_annotator

# part-of-speech (POS) tagging annotator
#' @importFrom openNLP Maxent_POS_Tag_Annotator
postag_annotator<- Maxent_POS_Tag_Annotator()
#' Return the singleton of part-of-speech (POS) tagging annotator
#' 
#' @return part-of-speech tagging annotator
#' @export
get.postag.annotator<- function() postag_annotator

#' Return table of token annotation.
#' 
#' Not exported.
#' @importFrom NLP Annotation
#' @importFrom openNLP Maxent_Word_Token_Annotator
tokenize.int<- function(string) {
  s<- as.String(string)
  annobj<- Annotation(1L, 'sentence', 1L, nchar(s))
  annotate(s, Maxent_Word_Token_Annotator(), annobj)
}

#' Tokenize the given string.
#' 
#' @param string string to tokenize
#' @export
tokenize<- function(string) {
  tokentbl<- tokenize.int(string)
  mapply(function(start, end) substr(string, start, end), 
         tokentbl$start[ tokentbl$type=='word'], 
         tokentbl$end[ tokentbl$type=='word'])
}

#' Tag the part-of-speech (POS) of the tokens in the given string sentence.
#' 
#' @param string sentence to tag
#' @return arrays of part-of-speech (tags used in Penn Treebank Project)
#' @importFrom NLP annotate
#' @export
tag.POS<- function(string) {
  tbl<- annotate(as.String(string), Maxent_POS_Tag_Annotator(), tokenize.int(as.String(string)))
  unlist(lapply(tbl$features, '[[', 'POS'))
}

#' Stem all tokens using Porter stemmer.
#' 
#' @param string string of sentence to stem
#' @return arrays of stemmed tokens
#' @importFrom SnowballC wordStem
#' @export
stem.all.tokens<- function(string) {
  tokentbl<- tokenize.int(as.String(string))
  stemmed.tokens<- mapply(function(start, end) wordStem(substr(string, start, end)), 
                          tokentbl$start[ tokentbl$type=='word'], 
                          tokentbl$end[ tokentbl$type=='word'])
  paste(stemmed.tokens, collapse = ' ')
}