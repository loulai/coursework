## Day 1
  * Finish up last week's classification exercises
  * Go through [this notebook](http://rpubs.com/jhofman/nb_vs_lr) comparing naive Bayes and logistic regression on the spam dataset from [Elements of Statistical Learning](http://statweb.stanford.edu/~tibs/ElemStatLearn/)
    * See [ROC Curve, Lift Chart, and Calibration plot](http://mrvar.fdv.uni-lj.si/pub/mz/mz3.1/vuk.pdf), [An introduction to ROC analysis](https://ccrma.stanford.edu/workshops/mir2009/references/ROCintro.pdf) and [interactive demo](http://www.navan.name/roc/) to understand ROC curves, precision, recall, etc.
    * See Figures 2 and 3 in [this paper](http://faculty.engr.utexas.edu/bickel/Papers/TWC_Calibration.pdf) for calibration plots for predicting rain
  * More on [Hypothetical Outcome Plots](https://medium.com/hci-design-at-uw/hypothetical-outcomes-plots-experiencing-the-uncertain-b9ea60d7c740) from [Jessica Hullman's](http://faculty.washington.edu/jhullman/) talk
  * [David Robinson](http://varianceexplained.org/about/) gave a guest lecture on his work as a data scientist at [Stack Exchange](https://en.wikipedia.org/wiki/Stack_Exchange)

## Day 2
  * Start the [Codecademy Python tutorial](https://www.codecademy.com/learn/python)
  * See [intro.py](intro.py) for in-class python examples
  * See [learnpython's advanced tutorials](http://www.learnpython.org) on generators and list comprehensions
  * Fill in the details of [stream_stats.py](stream_stats.py) to create a script that takes as input a text file with two tab-separated columns with one observation per line and outputs summary statistics for each group in the data. The first column in the input file is a "key" that represents the group and the second column is a numeric value for the observation within that group. You'll implement several versions of this script:
    * First, compute the minimum, mean, and maximum value within each group, assuming that the observations are ordered arbitrarily
	* Next, modify this to compute the median within each group as well and comment on how this changes the memory usage of your program
	* Finally, assume that the data are given to you sorted by the key, so that all of a group's observations are listed consecutively within the file and comment on how this assumption changes the minimum memory footprint needed by your program
    * [Sample input](sample_input.tsv) and [output](sample_output.tsv) are provided, where the output gives the key followed by all statistics (min, median, mean, and max)
  * [Fernando](http://research.microsoft.com/jump/164338) gave a guest lecture on [how to read research papers](reading-papers.pptx)
  * Read [Exposure to ideologically diverse news and opinion on Facebook](http://www.sciencemag.org/content/348/6239/1130.abstract). Also check out the [supplemental material](http://www.sciencemag.org/content/348/6239/1130/suppl/DC1) and open sourced [data and code](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LDJ7MS)

## Day 3
  * See the [example](get_article_urls.py) we worked on in class for the [NYTimes API](https://developer.nytimes.com/), using the [requests](http://docs.python-requests.org/en/master/user/quickstart/) module for easy http functionality
  * Read the first three sections of [Zapier's Introduction to APIs](https://zapier.com/learn/apis/)
  * Read this [overview of JSON](http://code.tutsplus.com/tutorials/understanding-json--active-8817) and review the first two sections of this overview of [Python's json module](http://pymotw.com/2/json/)
  * Complete [Codecademy's API tutorial](https://www.codecademy.com/courses/50e5bc94ce7f5e4945001d31/)
  * Read the draft of [Matt Salganik's](http://www.princeton.edu/~mjs3/) book chapter (on Slack)
  * Write Python code to download the 1000 most recent articles from the NYTimes API by section of the newspaper:
  	* [Register](https://developer.nytimes.com/signup) for an API key for the [Article Search API](https://developer.nytimes.com/article_search_v2.json)
  	* Use the [API console](https://developer.nytimes.com/article_search_v2.json#/Console/GET/articlesearch.json) to figure out how to query the API by section (hint: set the ``fq`` parameter to ``section_name:business`` to get articles from the Business section, for instance), sorted from newest to oldest articles
  	* Once you've figured out the query you want to run, translate this to working python code
  	* Your code should take an API key, section name, and number of articles as command line arguments, and write out a tab-delimited file where each article is in a separate row, with ``section_name``, ``web_url``, ``pub_date``, and ``snippet`` as columns
  	* You'll have to loop over pages of API results until you have enough articles, and you'll want to remove any newlines from article snippets to keep each article on one line
  	* Finally, run your code to get articles from the Business and World sections of the newspaper