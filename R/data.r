#' Plant performance summarized by holes
#'
#' A dataset containing all the performance measurements taken in the field,
#' there are summarized by hole (3 plants by holes) which is my statistical unit.
#'
#' *Only this dataset should be use in analysis.* 
#' 
#'
#' @format A data frame with 3524 rows and 12 variables:
#' \describe{
#'   \item{ter:}{terrace ID, i.e., 2, 3 or 4}
#'   \item{plot:}{plot ID, from 5 to 15}
#'   \item{label:}{hole ID, from 281 to 1531}
#'   \item{ms:}{microsite type, either Open (bare soil) or Patch (under a nurse)}
#'   \item{com:}{community type, either Mono (monospecific) or Poly (multispecies
#'   communities)}
#'   \item{watering:}{watering, either No watered or Watered} 
#'   \item{species:}{species identity, either anthyllis (Anthyllis citisoides),
#'   dorycnium (Dorycnium pentaphyllum) or pistacia (Pistacia lentiscus)}
#'   \item{date:}{month and year of measurement, a yearmon class vector (from zoo
#'   package)}
#'   \item{surv:}{survival in the hole by species, the variables takes either 1,
#'   2/3, 1/3 or in monospecies community and either 1 or 0 in multispecies
#'   communities}
#'   \item{d:}{basal diameter of the main stem in mm. For a given plant, the
#'   repeated measurements were at the same height and angle. In monospecific
#'   communities, it is the average of the 3 individuals. In multispecies
#'   communities, it is the one individual.}
#'   \item{h:}{height of the vegetative part of the plant in cm. Same
#'   thing than above for Mono/Poly communitites.} 
#'   \item{hm:}{maximum height of the vegetative part of the plant in cm,
#'   measured by binding the stem. Same thing than above for Mono/Poly
#'   communitites.} 
#' }
#'
#' @source \url{}
"holes_data"