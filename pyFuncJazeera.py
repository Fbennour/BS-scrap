
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
      url = 'https://www.aljazeera.com/search/{}?page={}'.format(query, k)
      html = urlopen(url)
      bs = BeautifulSoup(html, 'html.parser')
      t = bs.find_all('a', attrs={'href': re.compile('news')})
      for p in t:
        if p.attrs['href'] not in l:
          l.append(p.attrs['href'])
    except:
      continue
  titles=  list()
  # texts =  list()
  data= {}
  for k in range(1,len(l)):
     url=l[k]
     try:
       requete = requests.get(url)
       page = requete.content # Récuprer le contenu de la page
       soup = BeautifulSoup(page,"html.parser")
       titre = soup.find("h1",{}).text
       # paragraphe = [pr.get_text() for pr in soup.find_all("p", {})]
       # Text = ''
       # for i in range(0,len(paragraphe)):
         # Text = str(Text + paragraphe[i]) + '\n '
       # texts.append(Text)
       titles.append(titre)
     except:
       continue
  data = {'titles' : titles  }# , 'texts' : texts}
  df = pd.DataFrame(data,columns=['titles' ]) #, 'texts'])
  return df


  
  
# def BBC_linart(query, f_index, l_index):
#   l = list()
#   for k in np.arange(f_index, l_index, 1).tolist():
#     try:
#       url = 'https://www.bbc.co.uk/search?q={}&page={}'.format(query, k)
#       html = urlopen(url)
#       bs = BeautifulSoup(html, 'html.parser')
#       t = bs.find_all('a', attrs={'href': re.compile('news')})
#       for p in t:
#         if p.attrs['href'] not in l:
#           l.append(p.attrs['href'])
#     except:
#       continue
#   return l
# 
# 
# 
# 





