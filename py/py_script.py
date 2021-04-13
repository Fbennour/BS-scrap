# Spacy & Text
import spacy
from spacy.lang.en import English
from spacy.lang.en.stop_words import STOP_WORDS
import string

# Data Manipulation
import numpy as np
import pandas as pd

# Scikit Learn
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.base import TransformerMixin
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn import metrics

def get_NE(text):
    import spacy
    import pandas as pd
    nlp = spacy.load('en',parse=True,tag=True, entity=True)
    doc = nlp(text)
    l1 = list()
    l2 = list()
    for k in doc.ents:
        l1.append(k.text)
        l2.append(k.label_)
    df = pd.DataFrame(list(zip(l1, l2)), columns =['Named Entity', 'Label'])
    return df
    
# Creating our tokenizer function
def spacy_tokenizer(sentence):
    import spacy
    from spacy.lang.en import English
    from spacy.lang.en.stop_words import STOP_WORDS
    import string
    # Load English tokenizer, tagger, parser, NER and word vectors
    parser = English()
    # Create our list of punctuation marks
    punctuations = string.punctuation
    # Create our list of stopwords
    stop_words = spacy.lang.en.stop_words.STOP_WORDS
    # Creating our token object, which is used to create documents with linguistic annotations.
    mytokens = parser(sentence)
    # Lemmatizing each token and converting each token into lowercase
    mytokens = [ word.lemma_.lower().strip() if word.lemma_ != "-PRON-" else word.lower_ for word in mytokens]
    # Removing stop words
    mytokens = [ word for word in mytokens if word not in stop_words and word not in punctuations ]
    # return preprocessed list of tokens
    return mytokens  
    
# Creating our tokenizer function
def doc_spacy_tokenizer(sentence):
    import spacy
    import string
    punctuations = string.punctuation
    nlp = spacy.load('en',parse=True,tag=True, entity=True)
    doc1 = nlp(sentence)
    l=list()
    for token in doc1:
        if token.lemma_ != "-PRON-" and token.text not in punctuations:
           l.append(token.lemma_)
    return l

def doc_spacy_tagger(sentence):
    import spacy
    import string
    punctuations = string.punctuation
    nlp = spacy.load('en',parse=True,tag=True, entity=True)
    doc1 = nlp(sentence)
    l=list()
    for token in doc1:
        if token.lemma_ != "-PRON-" and token.text not in punctuations:
           l.append(token.pos_)
    return l
    
def get_compound_pairs(sentence, verbose=False):
    """Return tuples of (multi-noun word, adjective or verb) for document."""
    import spacy
    import string
    punctuations = string.punctuation
    nlp = spacy.load('en',parse=True,tag=True, entity=True)
    doc = nlp(sentence)
    compounds = [tok for tok in doc if tok.dep_ == 'compound'] # Get list of compounds in doc
    compounds = [c for c in compounds if c.i == 0 or doc[c.i - 1].dep_ != 'compound'] # Remove middle parts of compound nouns, but avoid index errors
    tuple_list = []
    if compounds: 
        for tok in compounds:
            pair_item_1, pair_item_2 = (False, False) # initialize false variables
            noun = doc[tok.i: tok.head.i + 1]
            pair_item_1 = noun
            # If noun is in the subject, we may be looking for adjective in predicate
            # In simple cases, this would mean that the noun shares a head with the adjective
            if noun.root.dep_ == 'nsubj':
                adj_list = [r for r in noun.root.head.rights if r.pos_ == 'ADJ']
                if adj_list:
                    pair_item_2 = adj_list[0] 
                if verbose == True: # For trying different dependency tree parsing rules
                    print("Noun: ", noun)
                    print("Noun root: ", noun.root)
                    print("Noun root head: ", noun.root.head)
                    print("Noun root head rights: ", [r for r in noun.root.head.rights if r.pos_ == 'ADJ'])
            if noun.root.dep_ == 'dobj':
                verb_ancestor_list = [a for a in noun.root.ancestors if a.pos_ == 'VERB']
                if verb_ancestor_list:
                    pair_item_2 = verb_ancestor_list[0]
                if verbose == True: # For trying different dependency tree parsing rules
                    print("Noun: ", noun)
                    print("Noun root: ", noun.root)
                    print("Noun root head: ", noun.root.head)
                    print("Noun root head verb ancestors: ", [a for a in noun.root.ancestors if a.pos_ == 'VERB'])
            if pair_item_1 and pair_item_2:
                tuple_list.append((pair_item_1, pair_item_2))
    return tuple_list

    
    
   
