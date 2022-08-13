import requests
import os
session = requests.session()
url = 'https://users.premierleague.com/accounts/login/'

payload = {
 'password': 'rr3Rct&Ryq&!',
 'login': 'chris.brownlie@hotmail.co.uk',
 'redirect_uri': 'https://fantasy.premierleague.com/',
 'app': 'plfpl-web'
}

session.post(url, data=payload)

cookies = session.cookies.get_dict()

response = session.get('https://fantasy.premierleague.com/api/my-team/7330951')
content = response.content
text = response.text
