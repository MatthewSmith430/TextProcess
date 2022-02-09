#' @title pre_clean
#'
#' @description Cleaning function for TEXT data - for both WOS & Scopus
#' @param DATA Dataframe with TEXT_DATA column
#' @export
#' @return text data vector
pre_clean<-function(DATA){
  sw<-stopwords::stopwords("en", source = "snowball")
  wl<-c("and","the","our","that","table","figure","published","indicate","aims","article","indicates","analysis","high","system","results",
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
  wl2<-c("and","the","our","that","table","figure","analysis","high","system","identifying","typical",
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
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "co author", "coauthor")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "igi global", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "co authors", "coauthor")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "copyright  the author(s)", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "under the terms of the creative commons attribution 4.0 international license. further distribution of this work must maintain attribution to the author(s) and the published article's title, journal citation, and doi", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer science+business media, llc, part of springer nature", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "r&d", "researchanddevelopment")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "this is an open access article distributed under the terms of the creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited",
                                           "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "elsevier ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "copyright taylor", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "copyright inderscience", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "emerald publishing limited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "emerald group", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "informa uk limited, trading as taylor & francis group", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "taylor & francis group", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "taylor & francis", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "edp sciences and springer", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "edp sciences", "")
  #springer-verlag gmbh germany
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "springer international publishing ag", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "springer science+business media", "")
  #american society of
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "american society of civil engineers", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag gmbh germany", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer nature", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer science", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag berlin", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag gmbh", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag wein", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals, inc", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley & sons", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley", "")
  #
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "publishing limited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "the author(s)", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag gmbh austria, part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "business media, llc, part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag gmbh germany", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer nature", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer science", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag berlin", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag gmbh", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag wein", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals, inc", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley & sons", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "elsevier b.v.", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "cambridge university press", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "oxford university press", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "university press", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "publishing limited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "the author(s)", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "business media, llc, part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag gmbh germany", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag wien", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "società italiana di fisica", "")
  #
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer nature", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer science", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag berlin", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag gmbh", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer verlag wein", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley & sons ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "creative commons attribution license, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals, inc", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley & sons", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john wiley", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "editor’s message to special issue on network science", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "sage publications", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "inderscience enterprises ltd", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "elsevier inc.", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "_ copyright", "")
  
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "wiley periodicals", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "publishing limited", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "the author(s)", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "business media, llc, part of", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "part of", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "published by the american physical society ", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "published by", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "national academy of sciences", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "mdpi ag", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "licensee mdpi, basel, switzerland", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "mdpi, basel, switzerland", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "ieee", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "in this paper", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "all rights reserved", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "elsevier", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "under exclusive licence to", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "world scientific publishing company", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "this is an open access article", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "distributed under the terms of the creative commons attribution", "")
  #
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "akadémiai kiadó, budapest, hungary", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "akadémiai kiadó", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "springer-verlag berlin heidelberg", "")
  #
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "verlag gmbh", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "& francis group, llc", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "john , ltd", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "the authors", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "by the american academy of political and social science", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "by the american academy of political and social", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "indian academy of sciences", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "análisis de", "")
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, "creative commons", "")
  
  DATA$TEXT_DATA<-stringr::str_replace_all(DATA$TEXT_DATA, 
                                           "american society of", "")
  
  
  
  abstract1<-gsub('[[:punct:] ]+',' ',DATA$TEXT_DATA)
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

