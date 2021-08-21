# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#UVOZ POVRšIN DRžAV EVROPSKE UNIJE Z WIKIPEDIJE

uvozi.povrsine <- function() {
  link <- "https://en.wikipedia.org/wiki/European_Union"
  stran <- session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable plainrowheaders']") %>%
    .[[1]] %>% html_table(dec=".")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("drzava", "prestolnica", "pridruzitev", "prebivalci", "povrsina",
                        "gostota", "MEP")
  sl <- locale("sl", decimal_mark=",", grouping_mark=".")
  for (col in c("povrsina", "prebivalci", "gostota")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], locale=sl)  #stolpec gledamo kot seznam [[]] in zamenjamo mankajoce z na, na ostalih pa uporabimo sl
    }
  }
  tabela$prestolnica <- NULL   #vseh teh stoplcev ne potrebujem
  tabela$pridruzitev <- NULL
  tabela$prebivalci <- NULL
  tabela$gostota <- NULL
  tabela$MEP <- NULL
  return(tabela)
}

