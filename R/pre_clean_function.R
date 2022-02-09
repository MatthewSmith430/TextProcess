#' @title pre_clean
#'
#' @description Cleaning function for TEXT data - for both WOS & Scopus
#' @param DATA Dataframe with TEXT_DATA column
#' @export
#' @return text data vector
pre_clean<-function(DATA){
  TEXT_DATA<-tolower(DATA$TEXT_DATA)
  sw<-stopwords::stopwords("en", source = "snowball")
  wl<-c("and","the","our","that","table","figure","published","indicate","aims","article","indicates",#"analysis",
        #"high","system",
        "results",
        "for","are","also","more","has","project","due","lead","take","taking","accordingly","aforementioned",
        "must","have","should","this","projects","within","area","potential","account","accounts","aftermath","afterwards",
        "with","furthermore","PhD","will","our","available","technologies","write","wrote","undertake","undertaken","agreed",
        "additionally","in","number","research","first","different","potential","ask","asks","asked","albeit","unlike","whose",
        "which","patterns","use","one","new","via","major","recent","identified","found","whenever","argue","argues","argued",
        "approach","whether","capture","targets","applications","across","without","find","alike","alongside","ahead","according",
        "effects","specifically","phd","target","methods","support","developed","process","undertakes",
        "instance","yet","two","many","understand","proposal","make","range","developing","around",
        "rather","approaches","along","understanding","propose","much","low","field","characteristics",
        "however","extent","provides","techniques","study","three","already","require","required","writings",
        "applied","characterised","well","investigate","allow","method","known","small","readily",
        "amongst","need","often","order","develop","significant","wide","end","areas","involve","involves",
        "table","based","key","examining","properties","include","including","current","involved",
        "captures","important","positive","using","aim","possible","increase","particular","form",
        "negative","better","perspective","journal","far","may","now","even","extensive","help","implications",
        "case","therefore","can","et","al","made","way","specific","by","obtained","analyzed","analyze","implication",
        "used","role","set","studies","measures","address","facing","gather","gathered","gathers","invited","annual",
        "highly","thus","years","example","although","examines","examined","examine",
        "paper","provide","figures","tables","reserved","rights","elsevier","level","na")
  wl2<-c("and","the","our","that","table","figure",#"analysis",
         "high","system","identifying","typical",
         "for","are","also","more","has","project","due","lead",
         "must","have","should","this","projects","within","area","potential","attempt","attempts","attempted","existing",
         "with","furthermore","PhD","will","our","proposed","propose","proposes","available","exists","ideas","idea",
         "additionally","in","number","research","first","different","potential","note","notes","report","reports",
         "which","patterns","use","one","new","via","major","recent","identified","investigated","novel","consider",
         "approach","whether","capture","targets","applications","across","without","related","result","presented","seen",
         "effects","specifically","phd","target","methods","support","developed","process","background","present",
         "instance","yet","two","many","understand","proposal","make","range","developing","purpose","presents","demonstrates",
         "rather","approaches","along","understanding","propose","much","low","field","approach","this","holds","hold","demonstrate",
         "however","extent","provides","techniques","study","three","already","suggest","suggests","require","required","explore",
         "applied","characterised","well","investigate","allow","method","known","small","while","given","explores","findings",
         "amongst","need","often","order","develop","significant","wide","end","areas","represent","might","among","application",
         "table","based","key","examining","properties","include","including","current","results","notes","ad","hoc","studying",
         "captures","important","positive","using","aim","possible","increase","particular","form","showed","attribution","study",
         "better","perspective","journal","far","may","now","even","team","results","among","findings","author","authors","toward",
         "case","therefore","can","work","et","al","made","way","specific","by","indicate","amongst","increasingly","contribution",
         "used","role","set","studies","measures","highly","thus","years","example","suggest","show","become","increase",
         "paper","provide","figures","tables","reserved","rights","elsevier","collect","collected","instead","try")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "social network analysis","socialnetworkanalysis")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "co author", "coauthor")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "igi global", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "co authors", "coauthor")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "copyright  the author(s)", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "under the terms of the creative commons attribution 4.0 international license. further distribution of this work must maintain attribution to the author(s) and the published article's title, journal citation, and doi", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer science+business media, llc, part of springer nature", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "r&d", "researchanddevelopment")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "this is an open access article distributed under the terms of the creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited",
                                           "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "elsevier ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "copyright taylor", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "copyright inderscience", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "emerald publishing limited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "emerald group", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "informa uk limited, trading as taylor & francis group", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "taylor & francis group", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "taylor & francis", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "edp sciences and springer", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "edp sciences", "")
  #springer-verlag gmbh germany
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "springer international publishing ag", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "springer science+business media", "")
  #american society of
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "american society of civil engineers", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag gmbh germany", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer nature", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer science", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag berlin", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag gmbh", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag wein", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals, inc", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley & sons", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley", "")
  #
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "publishing limited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "the author(s)", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag gmbh austria, part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "business media, llc, part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag gmbh germany", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer nature", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer science", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag berlin", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag gmbh", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag wein", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals, inc", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley & sons", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "elsevier b.v.", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "cambridge university press", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "oxford university press", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "university press", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "publishing limited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "the author(s)", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "business media, llc, part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag gmbh germany", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag wien", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "società italiana di fisica", "")
  #
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer nature", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer science", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag berlin", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag gmbh", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer verlag wein", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley & sons ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals, inc", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley & sons", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john wiley", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "editor’s message to special issue on network science", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "sage publications", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "inderscience enterprises ltd", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "elsevier inc.", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "_ copyright", "")
  
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "wiley periodicals", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "publishing limited", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "the author(s)", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "business media, llc, part of", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "part of", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "published by the american physical society ", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "published by", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "national academy of sciences", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "mdpi ag", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "licensee mdpi, basel, switzerland", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "mdpi, basel, switzerland", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "ieee", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "in this paper", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "all rights reserved", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "elsevier", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "under exclusive licence to", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "world scientific publishing company", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "this is an open access article", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "distributed under the terms of the creative commons attribution", "")
  #
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "akadémiai kiadó, budapest, hungary", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "akadémiai kiadó", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "springer-verlag berlin heidelberg", "")
  #
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "verlag gmbh", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "& francis group, llc", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "john , ltd", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "the authors", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "by the american academy of political and social science", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "by the american academy of political and social", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "indian academy of sciences", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "análisis de", "")
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, "creative commons", "")
  
  TEXT_DATA<-stringr::str_replace_all(TEXT_DATA, 
                                           "american society of", "")
  
  
  
  abstract1<-gsub('[[:punct:] ]+',' ',TEXT_DATA)
  abstract1<-stringr::str_replace_all(abstract1, "[^[:alnum:]]", " ")
  abstract1<-stringr::str_remove_all(abstract1,"elsevier bb all rights reserved")
  abstract1<-stringr::str_remove_all(abstract1,"elsevier bv all rights reserved")
  abstract1<-stringr::str_remove_all(abstract1,"all rights reserved")
  abstract1<-stringr::str_remove_all(abstract1,"elsevier inc")
  abstract1<-stringr::str_remove_all(abstract1,"elsevier")
  abstract1<-stringr::str_remove_all(abstract1,"various types")
  abstract1<-stringr::str_remove_all(abstract1,"best knowledge")
  abstract1<-stringr::str_remove_all(abstract1,"past decades")
  abstract1<-stringr::str_remove_all(abstract1,"past decade")
  abstract1<-stringr::str_remove_all(abstract1,"last decades")
  abstract1<-stringr::str_remove_all(abstract1,"last decade")
  abstract1<-stringr::str_remove_all(abstract1,"existing literature")
  abstract1<-stringr::str_remove_all(abstract1,"publishing ltd")
  abstract1<-stringr::str_remove_all(abstract1,"springer verlag")
  abstract1<-stringr::str_remove_all(abstract1,"francis group llc")
  abstract1<-stringr::str_remove_all(abstract1,"group llc")
  abstract1<-stringr::str_remove_all(abstract1,"media llc")
  abstract1<-stringr::str_remove_all(abstract1,"de la")
  abstract1<-stringr::str_remove_all(abstract1,"allows us")
  abstract1<-stringr::str_remove_all(abstract1,"business media")
  abstract1<-stringr::str_remove_all(abstract1,"biomed central ltd")
  abstract1<-stringr::str_remove_all(abstract1,"media york")
  abstract1<-stringr::str_remove_all(abstract1,"shed light")
  abstract1<-stringr::str_remove_all(abstract1,"practical implications")
  abstract1<-stringr::str_remove_all(abstract1,"point view")
  abstract1<-tm::removeWords(abstract1,tm::stopwords("english"))
  abstract1<-tm::removeWords(abstract1,sw)
  abstract1<-tm::removeWords(abstract1,wl)
  abstract1<-tm::removeWords(abstract1,wl2)
  abstract1<-tm::removeNumbers(abstract1)
  abstract1<-stringr::str_squish(abstract1)
  abstract1<-stringr::str_replace_all(abstract1, "\\u00AE|\\u00a9|\\u2122", "")
  abstract1<-trimws(abstract1)
  
  abstract1b<-gsub('\\b\\w{1,1}\\b','',abstract1)
  abstract1b<-stringr::str_squish(abstract1b)
  
  
  return(abstract1b)
}

