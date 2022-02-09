#' @title bigram_identify
#'
#' @description This function identifies frequency of bigrams from text. 
#' @param text_data The text data where you want to apply the bigram analysis. 
#' @param freq_bigram The minimum frequency of bigrams included in the dataframe (0 will include all)
#' @export
#' @return Dataframe

bigram_identify<-function(text_data,freq_bigram){
  bi_gram<-tokenizers::tokenize_ngrams(text_data, 
                           n = 2L, 
                           n_min = 2L, simplify = TRUE
  ) 
  
  BIl<-list()
  for (i in 1:length(bi_gram)){
    bd<-bi_gram[[i]]
    bd1<-tibble::as_tibble(bd)
    bd2<-dplyr::group_by(bd1,value)
    bd3<-dplyr::add_tally(bd2)
    bd4<-dplyr::ungroup(bd3)
    bd5<-unique(bd4)
    BIl[[i]]<-bd5
  }
  
  bDF<-purrr::map_df(BIl,data.frame)
  bDF1<-dplyr::group_by(bDF,value)
  bDF2<-dplyr::summarise(bDF1,Frequency = sum(n))
  bDF3<-unique(bDF2)
  bDF4<-dplyr::filter(bDF3,Frequency>freq_bigram)
  
  return(bDF4)
}