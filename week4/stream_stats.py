#!/usr/bin/env python

import sys
import fileinput
import numpy as np
from collections import defaultdict

def find_mean(arr):
  return sum(arr)/float(len(arr))

def find_median(arr):
  arr = sorted(arr)
  length = len(arr)
  if(length%2 == 0):
    top_sum = arr[(length/2)-1] + arr[length/2]
    return(float(top_sum/2)) #returns a number calculated that forms the 'middle'
  else: 
    return(float(arr[length/2]))#simply returns the middle number
  
if __name__ == '__main__':
  
    sys.argv = ['stream_stats.py', 'sample_input.tsv']
    # check for input filename given as first argument
    if len(sys.argv) < 2:
        sys.stderr.write('reading input from stdin\n')
        
    # read input one line at a time
    i = 0
    dict = {}

    for line in fileinput.input():
      fields = line.rstrip('\n').split('\t')
      key=fields[0]
      dict[key] = []
    
    for line in fileinput.input():
        # split on tab to get the key and value for each line
        fields = line.rstrip('\n').split('\t')
        key = fields[0]
        value = fields[1]
        dict[key].append(int(value))
    
    for k in dict:
      minimum = min(dict[k])
      mean = find_mean(dict[k])
      maximum = max(dict[k])
      med = find_median(dict[k])
      print(sorted(dict[k]))
      print "%s\t%s\t%s\t%s" % ("min", "median", "mean", "max")
      print "%i\t%i\t%i\t%i\n" % (minimum, med, mean, maximum)

      
      
    

    # update statistics for the group correponding to this key
    # (minimum, median, mean, and maximum)
    # loop over each group and output the group's key and statistics
    #print"%s\t%s\t" (x , y)
    #print "\t".join(array)
    #ls = [str(li) for li in l]
    #zip(['lou', 'ham'], [1 , 500])
    #first, second, third = "t\".split(l) //splits array then assigns first, second, third. Not sure if syntax is right
    #[uppercase_word(vi) for vi in v]
    #dd['spam']['money'] += 1 //makes spam: {money:1}
    #set(article.split()) //returns unique words in the string 'article'
    #words - set(['in', 'the']) //takes away 'in' and 'the'
    #words.update(set['a', 'b'])
    
    # | python -m json.tool //use to format APIs
