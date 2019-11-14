# Natural Language Processing: Word Prediction
This data product takes in a word or a sentence and predicts the next word. The model is trained on 70% of the [dataset](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) and uses a stupid backoff model with ngrams ranging from 1-5. The application is deployed at this link. https://chrishan.shinyapps.io/finalwordprediction/ 

#### -- Project Status: Completed

## Objective
Using the user's input text to predict their next word is best illustrated by the [SwiftKey Keyboard](https://www.microsoft.com/en-us/swiftkey). By using machine learning, SwiftKey provides a convenient way to reduce typing and improve the speed of communication. Our goal is to create a crude version of SwiftKey Keyboard in which the user can input text and the algorithm returns a vector of words along with their respective predictive probabilities.

This project was originally completed in February 2019 as part of the Data Science Specialization Capstone course offered on Coursera by Johns Hopkins University. This report aims to explain the process from the initial data exploration to creating the data product with the shiny application.

### Methods Used
* Data Preprocessing
* Data Visualization
* Natural Language Processing
* Data Product Development

### Technologies Used
* R 
* Shiny
* Quanteda
* Dplyr
* Tidyr
* Data.table
* Plotly

## Getting Started

1. Clone this repo (for help see this [tutorial](https://help.github.com/articles/cloning-a-repository/)).
2. Download the dataset from [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
3. Run the load_and_save.R and normalize.R (be mindful of the file paths, make sure to change them as needed to fit your local machine)
4. Run model.R
5. Use the methods (backoff_stop() and backoff_nostop()) to try your own predictions

## Featured Analysis
* [RMarkdown Report](http://rpubs.com/chris_han/nlpwordprediction)
* [Shiny Application](https://chrishan.shinyapps.io/finalwordprediction/)

## Contact
* Please contact me at christopherhan@stat.tamu.edu for any questions or concerns!
