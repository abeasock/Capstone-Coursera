# Summary

----------------------------------------------------------------------------------

### Objective

This summary introduces the application built for the Capstone project for the Coursera Data Science specialization by John Hopkins University in partnership with SwiftKey.

The primary goal of this project is to build a predictive model and a Shiny app that is able to predict the next word of a user's input.

----------------------------------------------------------------------------------

### Data Processing

The training data is available [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) provided in the course was a set of three documents for four different languages: English, Russian, Finnish, and German. The three documents in each contained text from three three different sources: blogs, news, and twitter. 

Due to the large size of the files, a random sample of 25,000 lines was taken from each English file (`en_US.blogs.txt`, `en_US.news.txt`, `en_US.twitter.txt`). Using the R `tm` package, I constructed a corpus and performed several methods to clean the data such as converting text to lowercase, removing white space, special characters, numbers, punctuation, and profanity (using the Google badwords dataset). Using the `RWeka` package, 3 term-document matrics were used to create 2-grams, 3-grams, and 4-grams.

----------------------------------------------------------------------------------

### Algorithm

The prediction algorithm utilizes the _Simple Good Turing Algorithm_ developed by Gale And Simpson ([more info](http://www.grsampson.net/AGtf1.html)) and _Stupid Backoff_.

The last 3 words (or 2 if that's all that's input) are taken from the entered text. If there are matching 4-grams and 3-grams, the 3-gram probabilities are multipled by 0.4 (due to backing off 1 level) and then the matching 4-grams and 3-grams are combined and reordered by highest probability. The top three words are presented as next word predictions. 

If the 4-grams or 3-grams have less than three matches, then the 2-grams are used to fill in the remaining needed matches to make three predictions. The 2-grams were very inaccurate in the prediction testing, even when using backoff techniques, which is why only 4-grams and 3-grams are considered if available.

Finally, if there are no matches in the 4-grams, 3-grams, or 2-grams then the message "No prediction found" is outputed.

----------------------------------------------------------------------------------

To learn more about about the data processing and modeling, please read the "Final Report" document located under the "Documentation" tab.
