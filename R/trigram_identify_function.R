#' @title trigram_identify
#'
#' @description This function identifies frequency of trigrams from text. 
#' @param text_data The text data where you want to apply the trigram analysis. 
#' @param freq_bigram The minimum frequency of trigrams included in the dataframe (0 will include all)
#' @export
#' @return Dataframe

trigram_identify<-function(text_data,freq_bigram){
  tri_gram<-tokenizers::tokenize_ngrams(text_data, 
                           n = 3L, 
                           n_min = 3L, simplify = TRUE
  ) 
  
  tril<-list()
  for (i in 1:length(tri_gram)){
    td<-tri_gram[[i]]
    td1<-tibble::as_tibble(td)
    td2<-dplyr::group_by(td1,value)
    td3<-dplyr::add_tally(td2)
    td4<-dplyr::ungroup(td3)
    td5<-unique(td4)
    tril[[i]]<-td5
  }
  
  triDF<-purrr::map_df(tril,data.frame)
  triDF1<-dplyr::group_by(triDF,value)
  triDF2<-dplyr::summarise(triDF1,Frequency = sum(n))
  triDF3<-unique(triDF2)
  triDF4<-dplyr::filter(triDF3,Frequency>freq_bigram)
  
  return(triDF4)
}
