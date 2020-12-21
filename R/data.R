#' Ontology of biomes
#'
#' A hierarchical list of biomes and synonyms as grouped by the IUCN Habitats 
#' Classification Scheme Version 3.1.
#'
#' @format A data.frame (337x3) containing nested biomes and habitats
"biomes"

#' Ontology of insect conservation actions
#' 
#' A hierarchical list of insect conservation actions ranging from general 
#' approaches (e.g. "Land/water management") to specific actions (e.g. "use 
#' glazing treatments to reduce light spill from inside lit buildings).
#' 
#' @format A data.frame (523x7) containing nested conservation actions
"conservation_actions"

#' Example insect conservation dataset
#' 
#' A dataset containing a set of example articles on insect conservation
#' 
#' @format A data.frame with 500 rows of 11 variables:
#' \describe{
#'   \item{title}{the article title}
#'   \item{abstract}{the article abstract}
#'   \item{keywords}{database-tagged keywords}
#'   \item{authors}{article authors}
#'   \item{source}{journal title or other source of the article}
#'   \item{year}{year of publication}
#'   \item{volume}{volume of publication}
#'   \item{issue}{issue of publication}
#'   \item{startpage}{start page for article in publication}
#'   \item{endpage}{end page for article in publication}
#'   \item{doi}{the article Digital Object Identifier (DOI)}
#' }
"insect_gapmap"

#' Example pest studies dataset
#' 
#' A dataset containing a set of example articles on crop pests
#' 
#' @format A data.frame with 300 rows of 11 variables:
#' \describe{
#'   \item{title}{the article title}
#'   \item{abstract}{the article abstract}
#'   \item{keywords}{database-tagged keywords}
#'   \item{authors}{article authors}
#'   \item{source}{journal title or other source of the article}
#'   \item{year}{year of publication}
#'   \item{volume}{volume of publication}
#'   \item{issue}{issue of publication}
#'   \item{startpage}{start page for article in publication}
#'   \item{endpage}{end page for article in publication}
#'   \item{doi}{the article Digital Object Identifier (DOI)}
#' }
"pest_articles"