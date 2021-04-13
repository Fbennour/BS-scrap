
from urllib.request import urlopen
from bs4 import BeautifulSoup
import requests
import numpy as np
import pandas as pd
import re

# pagination starts with 0

def EMR_link(url):
  texts =  list()
  titles=  list()
  data= {}
  try:
    requete = requests.get(url) 
    page = requete.content # Récuprer le contenu de la page 
    soup = BeautifulSoup(page,"html.parser")
    titre = soup.find("h1",{}).text
    paragraphe = [pr.get_text() for pr in soup.find_all("p", {})]
    Text = ''
    for i in range(0,len(paragraphe)):
      Text = str(Text + paragraphe[i]) + '\n '
  except:
    pass
  data = {'Title' : titre , 'Text' : Text}
  df = pd.DataFrame(data,columns=['Title', 'Text'], index=[0])  
  return df  
