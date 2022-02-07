#' @title WOS_create_TD
#'
#' @description This creates a new TEXT_DATA column for Web of Science DF for JOURNAL articles
#' @param DF Web of Science dataframe
#' @param year year (the end year - all articles before the given year)
#' @export
#' @return Dataframe
WOS_create_TD<-function(DF, year){
  DATA1<-dplyr::filter(DF,publication_type=="J")
  DATA2<-dplyr::filter(DATA1,publication_year<year)
  DATA<-tidyr::unite("TEXT_DATA",article_title,abstract,remove=FALSE)
  
  return(DATA)
}

