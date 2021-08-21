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
















# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}