import nltk
import newspaper
from newspaper import Article, Config
import numpy as np

import requests
from bs4 import BeautifulSoup
import re
import urllib.parse
from urllib.parse import urlparse
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from nltk.tokenize import sent_tokenize, word_tokenize

import spacy

from string import punctuation
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn
from sklearn.feature_extraction.text import CountVectorizer
import numpy as np

from nltk.util import ngrams
from collections import Counter

from nltk.corpus import stopwords

from nltk.stem import WordNetLemmatizer 

import pprint

import os
from os import walk
from os import listdir
from os.path import isfile, join

from nltk.corpus import wordnet

# import lda
# from lda import guidedlda as glda
# from lda import glda_datasets as gldad



def req_GOOG(query = "Money laundering", pages = np.arange(0, 190, 10).tolist()):
    urls = list()
    for k in pages:
        urls.append(
        'https://www.google.com/search?q={}&tbs=ar:1&tbm=nws&ei=pVccX_jwLtCdkwWf26GwCg&start={}&sa=N&ved=0ahUKEwi415TD4-jqAhXQzqQKHZ9tCKY4tAEQ8NMDCIsB&biw=1366&bih=657&dpr=1'.format(query, k)
        )
    return urls
def Articles_by_topic(query, pages):
    g_clean = []
    urls= req_GOOG(query, pages)
    
    for url in urls:
        try:
            html = requests.get(url)
            soup = BeautifulSoup(html.text, 'lxml')
            a = soup.find_all('a')
            for i in a:
                k = i.get('href')
                try:
                    m = re.search("(?P<url>https?://[^\s]+)", k)
                    n = m.group(0)
                    rul = n.split('&')[0]
                    domain = urlparse(rul)
                    if(re.search('google.com', domain.netloc)):
                        continue
                    elif (("mondaq.com" in rul) or ("www.ft.com" in rul) or ("www-ft-com" in rul) or ("www.terrenusenergy.com" in rul)):
                        continue
                    else:
                        if rul not in g_clean:
                            g_clean.append(rul)
                except:
                    continue
                
        except Exception as ex:
            continue
    return g_clean
def get_news(query, pages):
    List=Articles_by_topic(query, pages)
    articles=[]
    texts    =  list()
    titles   =  list()
    source   =  list()
    search   =  list()
    keywords =  list()
    data= {}
    for url in List:
        try:
            config = newspaper.Config()
            config.keep_article_html = True
            article=Article(url,config=config)
            article.download()
            article.parse()
            #article.nlp()
            try:
              art=Article(url,config=config)
              art.download()
              art.parse()
              art.nlp()
              keywords.append(art.keywords)
            except:
              keywords.append(None)
            texts.append(article.text)
            titles.append(article.title)
            source.append(url)
            search.append(query)
        except:
            continue
    data = {'query' :query, 'source': source, 'titles' : titles, 'texts' : texts, 'keywords': keywords}
    df = pd.DataFrame(data,columns=['query', 'source', 'titles', 'texts', 'keywords'])
    return(df)
  


# def get_news(query, pages):
#     List=Articles_by_topic(query, pages)
#     articles=[]
#     texts    =  list()
#     titles   =  list()
#     source   =  list()
#     search   =  list()
#     keywords =  list()
#     data= {}
#     for url in List:
#         try:
#             config = newspaper.Config()
#             config.keep_article_html = True
#             article=Article(url,config=config)
#             article.download()
#             article.parse()
#             texts.append(article.text)
#             titles.append(article.title)
#             source.append(url)
#             search.append(query)
#         except:
#             continue
#         try:
#           config = newspaper.Config()
#           config.keep_article_html = True
#           article=Article(url,config=config)
#           article.download()
#           article.parse()
#           article.nlp()
#           keywords.append(article.keywords)
#         except:
#           keywords.append(None)
#     data = {'query' :query, 'source': source, 'titles' : titles, 'texts' : texts, 'keywords': keywords}
#     df = pd.DataFrame(data,columns=['query', 'source', 'titles', 'texts', 'keywords'])
#     return(df)

  
  
  
    
def article_by_line(text):
    lines = text.split("\n")
    non_empty_lines = [line for line in lines if line.strip() != ""]
    return(non_empty_lines)
def to_remove(topic_articles):
    L=[]
    i=0
    for article in topic_articles:
        lines=article_by_line(article[1])
        for word in to_remove_cases:
            for line in lines:
                if 0<=((line.upper()).find(word)):
                    lines.remove(line)
        if (len(lines)==0):
            topic_articles.remove(article)
            continue
        else:
            L.append(lines)
    return(L,topic_articles)
def restore_articles(L):
    L_restored=[]
    for article in L:
        L_restored.append("\n".join(article))
    return(L_restored)
