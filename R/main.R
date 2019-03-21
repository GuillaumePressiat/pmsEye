


#' Obtenir le format pour un fichier visual
#' 
#' 
#' @import stringfix
#' @import stringr
#' @import readxl
#' @import readr
#' @import dplyr
#' @examples 
#' # path = "~/Documents/data/lmd/vvslamda"
#' # file = "750712184.2019.1.valo.lamda.txt"
#' # format_v(path, file)
#' @export
format_v <- function(path, file){
  
  passe_file <- stringr::str_match_all(file, "([0-9]{9})\\.(20[0-9]{2})\\.([0-9]+)\\.(.*)\\.(txt|csv)") %>% 
    unlist
  
  annee = passe_file[3]
  type_v = passe_file[5]
  
  if (!file.exists(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx')){
    stop("Format non supporté pour l'instant")
  }
  
  #format <- readxl::read_excel('inst/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx') %>%
  format <- readxl::read_excel(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx') %>%
    dplyr::mutate(type = dplyr::case_when(grepl('date_', Variable) ~ 'D',
                                          grepl('\\$', Format) ~ 'c',
                                          grepl('\\.', Format) ~ 'n',
                                          TRUE ~ 'i'))
  
  
  an <- format$Variable
  vec <- format$type
  col_types <-  vec
  is_character <- vapply(col_types, is.character, logical(1))
  col_concise <- function(x) {
    switch(x,
           "_" = ,
           "-" = readr::col_skip(),
           "?" = readr::col_guess(),
           c = readr::col_character(),
           D = readr::col_date('%d/%m/%Y'),
           d = readr::col_double(),
           i = readr::col_integer(),
           l = readr::col_logical(),
           n = readr::col_number(),
           T = readr::col_datetime(),
           t = readr::col_time(),
           stop("Unknown shortcut: ", x, call. = FALSE)
    )
  }
  col_types[is_character] <- lapply(col_types[is_character], col_concise)
  
  at <- structure(
    list(
      cols = col_types
    ),
    class = "col_spec"
  )
  
  if (type_v != "valo_ssr"){
    delim = ifelse(annee < "17", ";", "\t")
  }
  if (type_v == "valo_ssr"){
    delim = ";"
  }
  return(list(table_formats = format, at = at, an = an, delim = delim, type_v = type_v))
}



#' Importer un fichier visual pmsi
#' @examples 
#' 
#' # path = "~/Documents/data/lmd/vvslamda"
#' # file = "750712184.2019.1.valo.lamda.txt"
#' # import_v(path, file)
#' 
#' # path = "~/Documents/data/vvs"
#' # file = "750712184.2018.12.valo.txt"
#' # import_v(path, file)
#' @export
import_v <- function(path, file, tolower_names = TRUE, ...) {
  
  # importer le format pris dans le pdf
  format = format_v(path, file)
  
  # observer les entêtes du fichier à lire
  entetes_reelles <- readr::read_lines(file.path(path, file), n_max = 1) %>% 
    stringr::str_split(format$delim) %>% 
    unlist() %>% 
    tolower
  
  nbl_nok = length(format$an) != length(entetes_reelles)
  col_not_id = !identical(tolower(format$an), entetes_reelles)
  
  # si le nb col est différent, on changera de stratégie (2)
  if (nbl_nok){
    warning('
Le nombre de colonnes ne correspond pas au format indiqué dans le pdf de specs utilisé dans ce package.
On lit le fichier tel quel avec ses entêtes')
  }
  
  # on avertie de différences mineures entre les noms de colonnes (cas observés éventuellement)
  if (col_not_id & !nbl_nok){
    
    noks <- which(entetes_reelles != tolower(format$an))
    warning('
Le fichier contient des entêtes ne correspondant pas au format indiqué 
dans le pdf de specs utilisé dans ce package :\n' %+%
              ('-- Colonne ' % % noks % % ':' % % entetes_reelles[noks] %,% ' au lieu de :' % % 
              tolower(format$an)[noks] %c% '\n'))
    
  }
  
  # Stratégie (2) en pratique : on lit le fichier et on formate en se basant sur les noms de colonnes
  if (nbl_nok || col_not_id){
    v <- readr::read_delim(file.path(path, file), delim = format$delim, 
                           col_types = readr::cols(.default = readr::col_character()), na = c('.'), ...) %>% 
      stringfix::tolower_names() %>% 
      dplyr::mutate_at(vars(starts_with('date')), lubridate::dmy) %>%
      dplyr::mutate_at(vars(starts_with('mnt')), as.numeric) %>% 
      dplyr::mutate_at(vars(starts_with('coef')), as.numeric) %>% 
      dplyr::mutate_at(vars(starts_with('coef')), as.numeric) %>% 
      dplyr::mutate_at(vars(starts_with('supp')), as.integer) %>% 
      dplyr::mutate_at(vars(starts_with('nb')), as.integer)
  }
  
  # Stratégie (1) idéale : les colonnes correspondent au pdf de specs, on utilise les formats inscrits dans le package.
  if (!col_not_id){
  v <- readr::read_delim(file.path(path, file), delim = format$delim, 
                         col_types = format$at, col_names = format$an, skip = 1, na = c('.'), ...) 
  }
  
  v <- dplyr::mutate_if(v, is.character, stringr::str_trim)

  
  if (format$type_v == "valo_ssr"){
    # Enlever les quotes dans la colonne GME (fait pour excel ?)
    v <- dplyr::mutate(v, GME = stringr::str_remove(GME, "'"))
  }
  
  if (tolower_names == TRUE){
   return(stringfix::tolower_names(v))
  }
  return(stringfix::toupper_names(v))
}






#' Obtenir le dictionnaire des colonnes
#' @examples
#' # dico_v(2017, 'valo')
#' # dico_v(2019, 'valo.lamda')
#' @export
dico_v <- function(annee, type_v, tolower_names = TRUE) {
  if (!file.exists(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx')){
    stop("Format non supporté pour l'instant")
    return(NULL)
  }
  
  if (tolower_names == TRUE){
  return(dplyr::mutate(readxl::read_excel(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx'), 
                       Variable = tolower(Variable),
                       libelle = libelle %rmall% '\\"'))
  }
  return(dplyr::mutate(readxl::read_excel(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx'), 
                       Variable = toupper(Variable),
                       libelle = libelle %rmall% '\\"'))
}

#' Ouvrir le pdf de documentation du logiciel ATIH
#' @examples
#' # pdf_v(2017, 'valo')
#' # pdf_v(2019, 'valo.lamda')
#' @export
ouvrir_pdf_v <- function(annee, type_v){
  if (!file.exists(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.xlsx')){
    stop("Format non supporté pour l'instant")
    return(NULL)
  }
  if (!file.exists(path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% '.pdf')){
    stop("Pdf non historisé dans ce package")
    return(NULL)
  }
  system("open \"" %+% path.package('pmsEye') %+% '/formats/' %+% type_v %+% '/' %+% annee %+% ".pdf\"")
}
