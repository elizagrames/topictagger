## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(topictagger)

## -----------------------------------------------------------------------------
data("insect_gapmap", package = "topictagger")
knitr::kable(t(insect_gapmap[1, ]), "pipe")

## -----------------------------------------------------------------------------
articles <-
  tolower(
    paste(
      insect_gapmap$title,
      insect_gapmap$abstract,
      insect_gapmap$keywords,
      sep = " ;;; "
    )
  )
knitr::kable(articles[1], "pipe")

## -----------------------------------------------------------------------------
data("biomes", package = "topictagger")
knitr::kable(head(biomes, 10), "pipe")

data("conservation_actions", package = "topictagger")
knitr::kable(head(conservation_actions), "pipe")

## -----------------------------------------------------------------------------
biomes <- fill_rows(biomes)
knitr::kable(head(biomes, 10), "pipe")

conservation_actions <- fill_rows(conservation_actions)
knitr::kable(head(conservation_actions), "pipe")

## -----------------------------------------------------------------------------
biome_ontology <- create_dictionary(biomes, 
                                    return_dictionary = TRUE)

head(biome_ontology$`Artificial - Aquatic`)

action_ontology <- create_dictionary(conservation_actions, 
                    return_dictionary = TRUE)

head(action_ontology$`Law & policy`$`Private sector standards & codes`)

## -----------------------------------------------------------------------------
actions <- tag_strictly(doc = articles,
                        scheme = action_ontology,
                        allow_multiple = FALSE)
head(actions[!is.na(actions)])

habitats <- tag_strictly(doc = articles,
                         scheme = biome_ontology,
                         allow_multiple = FALSE)
head(habitats[!is.na(habitats)])

## -----------------------------------------------------------------------------
tags <- extract_levels(actions, n.levels = 2)
knitr::kable(sort(table(unlist(tags)), decreasing = T), "pipe")

habitat_tags <- extract_levels(habitats, n.levels = 1)
knitr::kable(sort(table(unlist(habitat_tags)), decreasing = T), "pipe")

## -----------------------------------------------------------------------------
# extract the highest level of the conservation action ontology tags
level1_tags <- extract_levels(actions, n.levels = 1)[[1]]

# separate our documents into known tags and unknown tags
tagged_documents <- articles[which(!is.na(level1_tags))]
unknown_documents <- articles[which(is.na(level1_tags))]

## -----------------------------------------------------------------------------
data("pest_articles", package = "topictagger")
knitr::kable(t(pest_articles[1, ]), "pipe")
pest_articles <-
  tolower(
    paste(
      pest_articles$title,
      pest_articles$abstract,
      pest_articles$keywords,
      sep = " ;;; "
    )
  )[1:250]

tagged_documents <- append(tagged_documents, pest_articles)

level1_tags <- append(level1_tags[!is.na(level1_tags)], 
                      rep("noise", length(pest_articles)))


## -----------------------------------------------------------------------------
knitr::kable(table(level1_tags), "pipe")

## -----------------------------------------------------------------------------
human_dimensions <- c(
  "Livelihood, economic & other incentives",
  "Education & awareness",
  "Law & policy",
  "Land/water protection"
)

level1_tags[level1_tags %in% human_dimensions] <-
  "Human dimensions" 

## -----------------------------------------------------------------------------
smart_tags <- tag_smartly(new_documents = unknown_documents,
                          tagged_documents = tagged_documents,
                          tags = level1_tags)

## -----------------------------------------------------------------------------
topics <-
  tag_freely(
    append(articles, pest_articles),
    k = 3,
    ngrams = TRUE,
    n_terms = 20
  )

# How are our documents distributed across topics?
barplot(
  table(topics[[1]]),
  las = 1,
  col = "white",
  xlab = "Topics",
  main = ""
)


## -----------------------------------------------------------------------------
knitr::kable(topics[[2]], "pipe")

