source_python("py/py_script.py")
#  1.3 Functions  ----
#  1.3.1 Use Document-term matrix to model Semantic clusters ----
#______This function receives Document-term matrix and range of topic's number and returns all models, 
#    a plot of perplexity and the selected model via number of desirable topics to be allowed
LDA_optimal<-function(dtm, k_min, k_max, k_select=2, iter = 500, alpha = 0.5, seed = 1234, thin = 1){
  models <- list()
  
  for (k in k_min:k_max) {
    l= list()
    l$k<- k
    l$model           <-  mod <- topicmodels::LDA(x=dtm, method="Gibbs", k=k,
                                                  control=list(alpha=alpha, seed=seed, iter=iter, thin=thin))
    l$log_likelihood  <- logLik( l$model )
    l$perplexity      <- perplexity(object= l$model , newdata=dtm)
    models[[length(models)+1]] <- l
  }
  
  topics=c()
  log_likelihood = c()
  perplexity=c()
  for (k in models) {
    l = unlist(k)
    topics = c(topics, l$k)
    log_likelihood = c(log_likelihood, l$log_likelihood)
    perplexity = c(perplexity, l$perplexity)
    
  }
  
  Eval_mod    =    data.frame(
    topics,
    log_likelihood,
    perplexity
  ) %>% 
    dplyr::mutate(diff = c(NA, rev(diff(rev(perplexity)))))
  
  
  p <- plotly::ggplotly(
    ggplot2::ggplot(Eval_mod,aes(x=topics, perplexity)) +
      geom_point() +
      geom_line())
  min_perp <- unlist(models[which(Eval_mod$perplexity == min(Eval_mod$perplexity))])
  max_perp <- unlist(models[which(Eval_mod$perplexity == max(Eval_mod$perplexity))])
  selected_mod <- unlist(models[which(Eval_mod$diff == min(abs((Eval_mod$diff)), na.rm=TRUE))])
  stat_perp <- unlist(models[which(Eval_mod$topics == k_select)])
  
  res = list()
  res$Plot = p
  res$min_perp = unlist(min_perp)
  res$max_perp =  unlist(max_perp)
  res$selected_mod = unlist(selected_mod)
  res$stationary_prep = unlist(stat_perp)
  
  return(res)
}
#  1.3.2 Create Document-term matrix ----
#______This function receives a dataframe that contains document IDs and each token in a row line and a
#    dataframe containing stopwords in a column whose the name is "word" exclusively
df_to_matrix <- function(df, stopwords){
  df %>%
    purrr::set_names('id','word') %>%
    dplyr::anti_join(stopword) %>%
    dplyr::count(id, word) %>% 
    tidytext::cast_dtm(document=id, term=word, value=n)
}
#  1.3.3 Remove all entities from a text ----
#______This function takes a text paragraph and a vector of entities and removes them from the text
remove_NE_from_text <- function(raw_text, entities){
  if(length(entities)>0){
    text <- raw_text
    for (k in entities) {
      txt <- stringr::str_replace_all(text, stringr::fixed(as.character(k)), ' ')
      text <- txt
    }
  }else {
    txt <- raw_text
  }
  return(txt)
}
#  1.3.4 Get all tokens with their tags based on Spacy models ----
intelligent_tokenization <- function(raw_text){
  
  article = raw_text  %>%
    replace_contraction()
  
  entites = get_NE(article) 
  
  text_with_no_entities = remove_NE_from_text(article,entites[,"Named Entity"])
  
  tokens = c(text_with_no_entities %>%
    doc_spacy_tokenizer())
  
  tags = c(text_with_no_entities %>%
    doc_spacy_tagger())
  
  names(entites) <- c("Words", "Label")
  df = data.frame(
    Words = tokens,
    tag = tags
  ) %>%
    mutate(Label = ifelse(
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", Words,
            ignore.case=TRUE),'E-mail adress', tag)) %>%
    select(-tag) %>%
    mutate_at('Words' ,trimws) %>%
    rbind(entites) %>% 
    filter(!Words %in% c('',NA,'\n'))
  return(df)
}
#  1.3.5 Tokenize naively a text  ----
tokenize_text <- function(text, id, stop_words){
  df <- intelligent_tokenization(as.character(text)) %>%
    set_names('word','Label') %>%
    mutate(word = textstem::lemmatize_words(word)) %>%
    mutate(word= tolower(word)) %>%
    anti_join(stop_words) %>%
    filter(!str_detect(word, "\\d"))%>%
    filter(nchar(word)>2)
  
  df <- df %>%
    mutate(id_doc = rep(id, nrow(df)))
  return(df)
}
#  1.3.6 Replace named entities with single term  ----
group_entitiesInText <- function(raw_text, entities, replacements){
  if(length(entities)>0){
    text <- raw_text
    for (k in 1:length(entities)) {
      txt <- stringr::str_replace_all(text, stringr::fixed(entities[k]), replacements[k])
      text <- txt
    }
  } else {
    txt <- raw_text
  }
  
  return(txt)
}
# ---- 2. Text segmentation and clustering  ----
#  2.1 Subset paragraph from a raw text  ----
SegmentizeText <- function(Corpus,text_column, pattern){
    Corpus  %>%
    rowid_to_column(var = "text_id") %>%
    mutate(paragraph_text = str_split(Corpus[,text_column], pattern = pattern)) %>%
    select(-text_column) %>%
    unnest(paragraph_text) %>%
    rowid_to_column(var = "paragraph_num") %>%
    select(text_id, paragraph_num, paragraph_text) %>%
    mutate(NCHAR = nchar(paragraph_text))  
}
#  2.2 List Named Entities of each paragraph  ----
Extract_Named_Entities <- function(df){
  Q <- data.frame()
  for (k in 1:nrow(df)) {
    NE = get_NE(df$paragraph_text[k])
    if(nrow(NE>0)){
      Q <- Q %>%
        rbind(
          data.frame(paragraph_num = df[k,'paragraph_num'],get_NE(df$paragraph_text[k]))
        )
    }
  }
  Q <- Q %>%
    mutate(NamedEntity = str_replace(`Named.Entity`,"\\+\\-\\.\\;\\:",'')) %>%
    mutate(NamedEntity = str_remove_all(NamedEntity," ")) %>%
    mutate(NamedEntity = str_remove_all(NamedEntity,"'s$")) %>%
    mutate(NamedEntity = str_remove_all(NamedEntity,"^the"))%>%
    mutate(NamedEntity = str_remove_all(NamedEntity,"^The"))%>%
    mutate(NamedEntity = str_trim(NamedEntity))
  return(Q %>% filter(!is.na(paragraph_num)))
}
#  2.3 Deal with Named entities  ----
NE_Cleansing <- function(df, text_id,text_column, group = FALSE, rm=TRUE, Extracted_Entities_table){
  NEWS <- df %>%
    mutate(TEXT = '-')
  if(rm==TRUE && group==FALSE){
    for (k in 1:nrow(NEWS)) {
      art_id = as.numeric(NEWS[k,text_id])
      NEWS$TEXT[k] <- remove_NE_from_text(NEWS[ ,text_column][NEWS[,text_id]==art_id]
                                          ,Extracted_Entities_table[Extracted_Entities_table[,text_id]==art_id,'Named.Entity'])
    }
  }else if(rm==FALSE && group==TRUE){
    for (k in 1:nrow(NEWS)) {
      art_id = as.numeric(NEWS[k,text_id])
      NEWS$TEXT[k] <- group_entitiesInText(NEWS[ ,text_column][NEWS[,text_id]==art_id]
                                           ,Extracted_Entities_table[Extracted_Entities_table[,text_id]==art_id,'Named.Entity']
                                           ,Extracted_Entities_table[Extracted_Entities_table[,text_id]==art_id,'NamedEntity'])
    }  
  }
  return(NEWS)
}
#  2.4 Clustering paragraphs into segments  ----
Clustering_doc_paragraphs <- function(doc_df,doc_id){
  df = doc_df  %>%
    set_names('id','text') %>%
    unnest_tokens(input=text, output=word) %>% 
    mutate(word_lemma = textstem::lemmatize_words(word)) %>%
    mutate(word_low= tolower(word_lemma)) %>%
    select(-word, -word_lemma) %>%
    set_names('id','word') %>%
    anti_join(stop_words) %>%
    filter(!str_detect(word, "\\d"))%>%
    filter(nchar(word)>2) %>%
    count(id, word) %>%
    bind_tf_idf(word, id, n) %>%
    arrange(desc(tf_idf))
  quant = quantile(df$tf_idf)
  df = df %>% 
    filter(tf_idf >= quant[1])%>% 
    cast_dtm(document=id, term=word, value=n) 
  l = LDA_optimal(df,2,10)
  df2 = tidy(l$min_perp$model, "gamma") %>% 
    spread(topic, gamma) %>%
    as.data.frame()
  tot_withinss <- map_dbl(1:(nrow(df2)-1),  function(k){
    model <- kmeans(x = df, centers = k)
    model$tot.withinss
  })
  elbow_df <- data.frame(
    k = 1:(nrow(df2)-1),
    tot_withinss = tot_withinss
  ) %>% 
    dplyr::mutate(diff = c(NA, rev(diff(rev(tot_withinss))))) %>%
    filter(diff == max(diff,na.rm = TRUE)) %>%
    select(k) %>%
    as.numeric()
  model_km <- kmeans(x = df, centers = elbow_df)
  df2 <- mutate(df2, cluster = paste(paste("Seg-",doc_id,sep = ''), model_km$cluster, sep = '-'))
  
  return(df2  %>%select(document,cluster) %>% set_names("paragraph_num","cluster"))
}
#  2.5 segments
all_seg <- function(art_rm_NE, text_ID, par_num, text){
  Q <- data.frame()
  art_rm_NE <- art_rm_NE %>%
    select_(text_ID,  par_num, text) %>%
    set_names('text_id','paragraph_num', 'TEXT')
  for (k in unique(art_rm_NE$text_id)) {
    Q <- Q %>% 
      rbind(
        Clustering_doc_paragraphs(
          art_rm_NE %>% 
            filter(text_id == k) %>%
            as.data.frame() %>%
            select(paragraph_num,TEXT),
          k)
      )
  }
  
  return(Q)
}



# ----  Jenson Shaanon Distribution  ----
  
computeJSD <- function(DOC1, DOC2, DocVsTopic){
  DF_TOPIC_DOC = as.data.frame(DocVsTopic)
  P <- as.matrix(DF_TOPIC_DOC[DF_TOPIC_DOC$document == DOC1, 2:ncol(DF_TOPIC_DOC)])
  Q <- as.matrix(DF_TOPIC_DOC[DF_TOPIC_DOC$document == DOC2, 2:ncol(DF_TOPIC_DOC)])
  x <- rbind(P,Q)
  x <- rbind(P,Q)
  return(JSD(x))
}

get_similarties <- function(DOC,DocVsTopic) {
  DF_TOPIC_DOC = as.data.frame(DocVsTopic) 
  Sim_Doc = c()
  names = c()
  for (k in 1:nrow(DF_TOPIC_DOC_)) {
    if(DF_TOPIC_DOC$document[k]!=DOC){
      Sim_Doc[k] <- computeJSD(DOC,DF_TOPIC_DOC$document[k],t1)
      names[k] <- DF_TOPIC_DOC$document[k]
    }
  }
  
  ds= data.frame(names,Sim_Doc)
  names(ds) <- c(paste0("Similar to",DOC,sep = ' '), "Similarity" )
  
  return(dplyr::ds[-1,] %>% arrange(desc(Similarity)))
}




### ----- LDA OPTIMAL NEW ----

LDA_optimal_table<-function(dtm, k_min, k_max, k_select=2, iter = 500, alpha = 0.5, seed = 1234, thin = 1){
  models <- list()
  
  for (k in k_min:k_max) {
    l= list()
    l$k<- k
    l$model           <-  mod <- topicmodels::LDA(x=dtm, method="Gibbs", k=k,
                                                  control=list(alpha=alpha, seed=seed, iter=iter, thin=thin))
    l$log_likelihood  <- logLik( l$model )
    l$perplexity      <- perplexity(object= l$model , newdata=dtm)
    models[[length(models)+1]] <- l
  }
  
  topics=c()
  log_likelihood = c()
  perplexity=c()
  for (k in models) {
    l = unlist(k)
    topics = c(topics, l$k)
    log_likelihood = c(log_likelihood, l$log_likelihood)
    perplexity = c(perplexity, l$perplexity)
    
  }
  
  Eval_mod    =    data.frame(
    topics,
    log_likelihood,
    perplexity
  ) %>% 
    dplyr::mutate(diff = c(NA, rev(diff(rev(perplexity))))) %>%
    mutate(diff = abs(diff))
  
  
  
  return(Eval_mod)
}



#### ADDED BY ME----
##--wordcloud function ----
create_wordcloud <- function(article){
  art_parg= data.frame(
    paragraph_text =unlist(tokenize_sentence(article))
  ) %>%
    rowid_to_column(var = "paragraph_num")
  
  
  art_rm_NE = NE_Cleansing(art_parg, 'paragraph_num', 'paragraph_text', group = TRUE, rm=FALSE,    Extract_Named_Entities(art_parg) %>% filter(Label %in% c("GPE", "ORG", "PERSON","LOC",'NORP')) %>% select(-Label) %>% unique())
  
  df = art_rm_NE %>%
    select(paragraph_num, TEXT) %>%
    unnest_tokens(input = TEXT, output = word)  %>%
    mutate(word = str_remove_all(word,"'s$")) %>%
    mutate(word = str_remove_all(word,"^the"))%>%
    mutate(word = str_remove_all(word,"^The"))%>%
    mutate(word = textstem::lemmatize_words(word)) %>%
    mutate(word= tolower(word)) %>%
    filter(!str_detect(word, '^\\d')) %>%
    filter(!str_detect(word, '^\\d[a-z][a-z]')) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>%
    purrr::set_names('id','word') %>%
    dplyr::count(id, word)
  
  
  df_lemma = df %>%
    mutate(word = str_remove_all(word,"'s$")) %>%
    mutate(word = str_remove_all(word,"^the"))%>%
    mutate(word = str_remove_all(word,"^The"))%>%
    mutate(word = textstem::lemmatize_words(word)) %>%
    mutate(word= tolower(word)) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>%
    purrr::set_names('id','word','n') %>%
    dplyr::count(id, word)
  
  dtm = df_lemma %>%
    tidytext::cast_dtm(document=id, term=word, value=n)
  # new LDA_OPTIMAL 
  mod1 = LDA_optimal_table(dtm, 2, 10)
  min_perp <- unlist(mod1$topics[which(mod1$perplexity == min(mod1$perplexity))])
  k_optimal=    unlist(min_perp)
  
  mod = LDA_optimal(dtm, 2, 10, k_optimal)
  
  terms = terms(mod$min_perp$model, k=20) %>%
    as.data.frame()%>%
    gather( topic, word, 1:k_optimal, factor_key=FALSE) %>%
    left_join(
      df
    ) %>%
    filter(!is.na(n)) %>%
    select(-id) %>%
    group_by(topic, word) %>%
    mutate(n = sum(n))%>%
    unique()
  
  
  wordcloud(words = terms$word, freq = terms$n, 
            # min.freq=input$freq, 
            # max.words=input$max, 
            min.freq=1, max.words=40,
            random.order=FALSE, rot.per=0.35,
            ordered.colors=TRUE,
            colors=brewer.pal(8, "Dark2")[factor(terms$topic)], size=1.5)
}

##### worrdcloud2


create_wordcloud2 <- function(article){
  # doc_id=news[Body==article, doc_id]
  art_parg= data.frame(
    paragraph_text =unlist(tokenize_sentence(article))
  ) %>%
    rowid_to_column(var = "paragraph_num")
  
  
  art_rm_NE = NE_Cleansing(art_parg, 'paragraph_num', 'paragraph_text', group = TRUE, rm=FALSE,    Extract_Named_Entities(art_parg) %>% filter(Label %in% c("GPE", "ORG", "PERSON","LOC",'NORP')) %>% select(-Label) %>% unique())
  
  df = art_rm_NE %>%
    select(paragraph_num, TEXT) %>%
    unnest_tokens(input = TEXT, output = word)  %>%
    mutate(word = str_remove_all(word,"'s$")) %>%
    mutate(word = str_remove_all(word,"^the"))%>%
    mutate(word = str_remove_all(word,"^The"))%>%
    mutate(word = textstem::lemmatize_words(word)) %>%
    mutate(word= tolower(word)) %>%
    filter(!str_detect(word, '^\\d')) %>%
    filter(!str_detect(word, '^\\d[a-z][a-z]')) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>%
    purrr::set_names('id','word') %>%
    dplyr::count(id, word)
  
  
  df_lemma = df %>%
    mutate(word = str_remove_all(word,"'s$")) %>%
    mutate(word = str_remove_all(word,"^the"))%>%
    mutate(word = str_remove_all(word,"^The"))%>%
    mutate(word = textstem::lemmatize_words(word)) %>%
    mutate(word= tolower(word)) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>%
    purrr::set_names('id','word','n') %>%
    dplyr::count(id, word)
  
  dtm = df_lemma %>%
    tidytext::cast_dtm(document=id, term=word, value=n)
  # new LDA_OPTIMAL 
  mod1 = LDA_optimal_table(dtm, 2, 10)
  min_perp <- unlist(mod1$topics[which(mod1$perplexity == min(mod1$perplexity))])
  k_optimal=    unlist(min_perp)
  
  mod = LDA_optimal(dtm, 2, 10, k_optimal)
  
  terms = terms(mod$min_perp$model, k=20) %>%
    as.data.frame()%>%
    gather( topic, word, 1:k_optimal, factor_key=FALSE) %>%
    left_join(
      df
    ) %>%
    filter(!is.na(n)) %>%
    select(-id) %>%
    group_by(topic, word) %>%
    mutate(n = sum(n))%>%
    unique()
  my_graph <- wordcloud2(terms[,2:3], size=1,fontWeight = "normal",shuffle = F, color= brewer.pal(8, "Dark2")[factor(terms$topic)])
  
  # webshot::install_phantomjs()
  # saveWidget(my_gr,"tmp.html",selfcontained = F)
  # webshot("tmp.html",paste0("wordcloud_",doc_id,".png"), delay =5, vwidth = 480, vheight=480)
  # 



}
##--- sent analysis function ----

create_sent_analysis <- function(article){
  source_python("py/pipeline_financial_sentiment.py")
  
  paragraph_text_tbl <- tibble(
    # Page Text
    page_text = article
  ) %>%
    rowid_to_column(var = "page_num") %>%
    
    # Paragraph Text
    mutate(paragraph_text = str_split(page_text, pattern = "\\.\n")) %>%
    select(-page_text) %>%
    unnest(paragraph_text) %>%
    rowid_to_column(var = "paragraph_num") %>%
    select(page_num, paragraph_num, paragraph_text)
  
  
  sentiment_classification <- paragraph_text_tbl %>%
    pull(paragraph_text) %>%
    pipeline_classification()
  
  sentiment_regression <- paragraph_text_tbl %>%
    pull(paragraph_text) %>%
    pipeline_regression()
  
  data_prepared_tbl <- paragraph_text_tbl %>%
    mutate(
      sentiment_classification = sentiment_classification,
      sentiment_regression     = sentiment_regression
    ) %>%
    mutate(label = str_glue("Page: {page_num}
                            Paragraph: {paragraph_num}
                            Sentiment: {round(sentiment_regression)}
                            ---
                            {str_wrap(paragraph_text, width = 80)}"))
  
  
  g <- data_prepared_tbl %>%
    mutate(sentiment_classification = case_when(
      sentiment_classification == 0  ~ "neutral",
      sentiment_classification == 1  ~ "positive",
      sentiment_classification == -1 ~ "negative"
    ) %>% factor(levels = c("negative", "neutral", "positive"))) %>%
    ggplot(aes(sentiment_classification, sentiment_regression, color = sentiment_regression)) +
    geom_point(aes(text = label,
                   size = abs(sentiment_regression))) +
    scale_color_viridis_c() +
    theme_tq() +
    coord_flip()
  
  g<-ggplotly(g, tooltip = "text")%>% plotly_build()

  }



###-- NE tab ----

NEtab <- function(article){

  #   datatable(get_NE(article), filter = 'top',  
  # options = list(pageLength = 15, autoWidth = TRUE),
  # rownames= FALSE )
  
  as.data.table(get_NE(article))
}
