


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
  
  passe_file <- stringr::str_match_all(file, "([0-9]{9})\\.(20[0-9]{2})\\.([0-9]+)\\.(.*)\\.txt") %>% 
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
  
  format = format_v(path, file)
  v <- readr::read_delim(file.path(path, file), delim = format$delim, 
                         col_types = format$at, col_names = format$an, skip = 1, na = c('.'), ...)

  
  if (tolower_names == TRUE){
   return(tolower_names(v))
  }
  return(toupper_names(v))
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
