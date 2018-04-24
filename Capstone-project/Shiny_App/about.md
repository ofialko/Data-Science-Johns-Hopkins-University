
This is the capstone project for [the Coursera Data Science specialization](https://www.coursera.org/specializations/jhu-data-science).
The goal of this project is to build a shiny application that is able to predict the next word. 
Text data used to train a model able to  predict the next word comes from [HC Corpora](http://www.corpora.heliohost.org/). 

The HC Corpora data were thoroughly cleaned: punctuations, numbers, URL, profanity words were removed.
After that the data were tokenized into *n*-grams, which is a contiguous sequence of n items from a given sequence of text or speech. 
Aggregated in this way 2-,3- and 4-grams were transformed  into frequency distributions and stored in separate data frames.
The application receives a text input and uses the n-grams data frames to predict the next word.

The code of this application and the milestone report can be found in [this Git-Hub repo](https://github.com/ofialko/Capstone-Coursera).


