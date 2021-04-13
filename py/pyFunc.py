
from urllib.request import urlopen
from bs4 import BeautifulSoup
import requests
import numpy as np
import pandas as pd
import re



def JZR_articles(query, f_index, l_index):
  l = list()
  for k in np.arange(f_index, l_index, 1).tolist():
    try:
      url = 'hhttps://www.bloomberg.com/search/{}'.format(query, k)
      html = urlopen(url)
      bs = BeautifulSoup(html, 'html.parser')
      t = bs.find_all('a', attrs={'href': re.compile('news')})
      for p in t:
        if p.attrs['href'] not in l:
          l.append(p.attrs['href'])
    except:
      continue
  titles=  list()
  texts =  list()
  data= {}
  for k in range(1,len(l)):
     url=l[k]
     try:
       requete = requests.get(url)
       page = requete.content # Récuprer le contenu de la page
       soup = BeautifulSoup(page,"html.parser")
       titre = soup.find("h1",{}).text
       paragraphe = [pr.get_text() for pr in soup.find_all("p", {})]
       Text = ''
       for i in range(0,len(paragraphe)):
         Text = str(Text + paragraphe[i]) + '\n '
       texts.append(Text)
       titles.append(titre)
     except:
       continue
  data = {'Titles' : titles , 'Body' : texts}
  df = pd.DataFrame(data,columns=['Titles', 'Body'])
  return df


  

# 
# 
# def BBC_link(link):
#   url= link
#   titles=  list()
#   texts =  list()
#   data= {}
#   try:
#     requete = requests.get(url)
#     page = requete.content # Récuprer le contenu de la page
#     soup = BeautifulSoup(page,"html.parser")
#     titre = soup.find("h1", {"id": "main-heading"}).text
#     paragraphe = [pr.get_text() for pr in soup.find_all("div", {"data-component": "text-block"})]
#     Text = ''
#     for i in range(0,len(paragraphe)):
#      Text = str(Text + paragraphe[i]) + '\n '
#     texts.append(Text)
#     titles.append(titre)
#   data = {'Titles' : titles , 'Body' : texts}
#   df = pd.DataFrame(data,columns=['Titles', 'Body'])
#   return df
# 


