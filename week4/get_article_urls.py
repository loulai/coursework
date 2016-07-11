#
# file: get_article_urls.py
#
# description: fetches article urls from the NYTimes API
#
# usage: get_articles.py <api_key>
#
# requirements: a NYTimes API key
#   available at https://developer.nytimes.com/signup
#

import requests
import json
import sys

ARTICLE_SEARCH_URL = 'https://api.nytimes.com/svc/search/v2/articlesearch.json'
api_key = '64bbda9b5dc04c40bc5d5095bef326d0'

if __name__=='__main__':
   if len(sys.argv) != 2:
      sys.stderr.write('usage: %s <api_key>\n' % sys.argv[0])
      sys.exit(1)

   api_key = sys.argv[1]

   params = {'api-key': api_key}
   r = requests.get("https://api.nytimes.com/svc/search/v2/articlesearch.json", params)
   data = json.loads(r.content)

   for doc in data['response']['docs']:
        print doc['web_url']

#codecs module utf-8 //unicode problems
