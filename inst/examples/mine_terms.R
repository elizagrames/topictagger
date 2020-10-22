doc <- "Recently burned coniferous forests host wildlife communities 
        that respond to variation in burn severity, post-fire habitat 
        structure, and patch configuration. Habitat selection theory 
        predicts that birds inhabiting these variable post-fire landscapes 
        will select nesting locations that confer an adaptive advantage 
        through increased fitness and reproductive success. Understanding 
        the effect of post-fire habitat on avian nesting ecology can 
        provide valuable information to guide restoration and management 
        after wildfire. The Black-backed Woodpecker (Picoides arcticus) 
        is strongly associated with recently burned forests in the western 
        United States, where it is used as an indicator species for the 
        effects of post-fire forest management."

keywords <- c("protection of habitat")

mine_terms(doc, retain_stopwords=TRUE, known_phrases=keywords)
