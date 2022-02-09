#' @title WOS_read
#'
#' @description This takes a list of excel files downloaded from Web of Science (WOS) and combines them into a single dataframe.
#' @param pattern File pattern name (such as "savedrecs")
#' @export
#' @return Dataframe
WOS_read<-function(pattern){
  FL1<-list.files(pattern=pattern)
  FL2<-list()
  
  for (i in 1:length(FL1)){
    df<-readxl::read_excel(FL1[[i]])
    df<-janitor::clean_names(df)
    df<-dplyr::select(df,-c("volume","x70","issue","start_page","end_page",
                "supplement"))
    FL2[[i]]<-df
  }
  
  DATA<-purrr::map_df(FL2,data.frame)
  
  return(DATA)
}

