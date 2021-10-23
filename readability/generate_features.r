
generate_feats <- function(my.model,new.text,lang = ud_eng){
  
  # Tokenization and document-feature matrix
  
  tokenized <- tokens(new.text,
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE)
  
  dm <- dfm(tokenized)
  
  # basic text stats
  
  text_sm <- textstat_summary(dm)
  text_sm$sents <- nsentence(new.text)
  text_sm$chars <- nchar(new.text)
  
  # Word-length features
  
  wl <- nchar(tokenized[[1]])
  
  wl.tab <- table(wl)
  
  wl.features <- data.frame(matrix(0,nrow=1,nco=30))
  colnames(wl.features) <- paste0('wl.',1:30)
  
  ind <- colnames(wl.features)%in%paste0('wl.',names(wl.tab))
  
  wl.features[,ind] <- wl.tab
  
  wl.features$mean.wl  <-   mean(wl)
  wl.features$sd.wl    <-   sd(wl)
  wl.features$min.wl   <-   min(wl)
  wl.features$max.wl   <-   max(wl)
  
  # Text entropy/Max entropy ratio
  
  t.ent <- textstat_entropy(dm)
  n     <-  sum(featfreq(dm))
  p     <- rep(1/n,n)
  m.ent <- -sum(p*log(p,base=2))
  
  ent <- t.ent$entropy/m.ent
  
  # Lexical diversity
  
  text_lexdiv <- textstat_lexdiv(tokenized,
                                 remove_numbers = TRUE,
                                 remove_punct   = TRUE,
                                 remove_symbols = TRUE,
                                 measure        = 'all')
  
  # Measures of readability
  
  text_readability <- textstat_readability(new.text,measure='all')
  
  # POS tag frequency
  
  annotated <- udpipe_annotate(lang, x = new.text)
  annotated <- as.data.frame(annotated)
  annotated <- cbind_morphological(annotated)
  
  pos_tags <- c(table(annotated$upos),table(annotated$xpos))
  
  # Syntactic relations
  
  dep_rel <- table(annotated$dep_rel)
  
  # morphological features
  
  feat_names <- c('morph_abbr','morph_animacy','morph_aspect','morph_case',
                  'morph_clusivity','morph_definite','morph_degree',
                  'morph_evident','morph_foreign','morph_gender','morph_mood',
                  'morph_nounclass','morph_number','morph_numtype',
                  'morph_person','morph_polarity','morph_polite','morph_poss',
                  'morph_prontype','morph_reflex','morph_tense','morph_typo',
                  'morph_verbform','morph_voice')
  
  feat_vec <- c()
  
  for(j in 1:length(feat_names)){
    
    if(feat_names[j]%in%colnames(annotated)){
      morph_tmp   <- table(annotated[,feat_names[j]])
      names_tmp   <- paste0(feat_names[j],'_',names(morph_tmp))
      morph_tmp   <- as.vector(morph_tmp)
      names(morph_tmp) <- names_tmp
      feat_vec  <- c(feat_vec,morph_tmp)
    }
  }
  
  # Sentence Embeddings
  
  embeds <- textEmbed(x     = new.text,
                      model = 'roberta-base',
                      layers = 12,
                      context_aggregation_layers = 'concatenate')
  
  
  # combine them all into one vector and store in the list object
  
  input <- cbind(text_sm[2:length(text_sm)],
                 wl.features,
                 as.data.frame(ent),
                 text_lexdiv[,2:ncol(text_lexdiv)],
                 text_readability[,2:ncol(text_readability)],
                 t(as.data.frame(pos_tags)),
                 t(as.data.frame(c(dep_rel))),
                 t(as.data.frame(feat_vec)),
                 as.data.frame(embeds$x)
  )
  
  # feature names from the model
  
  my_feats <- my.model$recipe$var_info$variable
  
  # Find the features missing from the new text
  
  missing_feats <- ! my_feats %in% colnames(input)
  
  # Add the missing features (with assigned values of zeros)
  
  temp           <- data.frame(matrix(0,1,sum(missing_feats)))
  colnames(temp) <- my_feats[missing_feats]
  
  input <- cbind(input,temp)
  
  return(list(input=input))
}