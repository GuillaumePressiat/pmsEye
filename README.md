# pmsEye


Import des sorties des logiciels Visual* de l'ATIH


## Exemples

```r
library(pmsEye)

# Obtenir le dictionnaire des colonnes
View(dico_v(2018, 'valo'))


file <- glue::glue('{finess}.{annee}.{mois}.valo.txt',
                   finess = '750712184',
                   annee = 2018,
                   mois = 12)

# Importer la table (1000 premières lignes ici)
import_v(
  path = '~/Documents/data/vvs',
  file =  file,
  n_max = 1e3
)

# ouvrir le manuel technique Visual* de l'ATIH
ouvrir_pdf_v(2018, 'valo')
```
