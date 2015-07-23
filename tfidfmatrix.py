# 7/21/2015
# This script :
#
# * reads in the Yelp review data,
# * combines the review data with the city of Boston's health inspection file
# * extracts the top N features using the TF-IDF method, for both the training set and test set. 
# 
# Data was obtaind after registering for DrivenData's Keeping It Fresh Competition: http://www.drivendata.org/competitions/5/
# This script was developed by DrivenData, is explained in this blogpost: http://blog.drivendata.org/2015/05/07/keeping-it-fresh-benchmark/
# Lines were added to export the TF-IDF matrix to CSV so that additional processing and model development
# could be done in R 

import numpy as np
import pandas as pd


import warnings
warnings.filterwarnings("ignore")

id_map = pd.read_csv("restaurant_ids_to_yelp_ids.csv")
id_dict = {}

# each Yelp ID may correspond to up to 4 Boston IDs
for i, row in id_map.iterrows():
    # get the Boston ID
    boston_id = row["restaurant_id"]
    
    # get the non-null Yelp IDs
    non_null_mask = ~pd.isnull(row.ix[1:])
    yelp_ids = row[1:][non_null_mask].values
    
    for yelp_id in yelp_ids:
        id_dict[yelp_id] = boston_id

with open("yelp_academic_dataset_review.json", 'r') as review_file:
    review_json = '[' + ','.join(review_file.readlines()) + ']'

# read in the json as a DataFrame
reviews = pd.read_json(review_json)

# drop columns that we won't use
reviews.drop(['review_id', 'type', 'user_id', 'votes'], 
             inplace=True, 
             axis=1)

# replace yelp business_id with boston restaurant_id
map_to_boston_ids = lambda yelp_id: id_dict[yelp_id] if yelp_id in id_dict else np.nan
reviews.business_id = reviews.business_id.map(map_to_boston_ids)

# rename first column to restaurant_id so we can join with boston data
reviews.columns = ["restaurant_id", "date", "stars", "text"]    

# drop restaurants not found in boston data
reviews = reviews[pd.notnull(reviews.restaurant_id)]

reviews.head()

train_labels = pd.read_csv("training.csv", index_col=0)
submission = pd.read_csv("SubmissionFormat.csv", index_col=0)

def flatten_reviews(label_df, reviews):
    """ label_df: inspection dataframe with date, restaurant_id
        reviews: dataframe of reviews
        
        Returns all of the text of reviews previous to each
        inspection listed in label_df.
    """
    reviews_dictionary = {}
    
    N = len(label_df)

    for i, (pid, row) in enumerate(label_df.iterrows()):
        # we want to only get reviews for this restaurant that ocurred before the inspection
        pre_inspection_mask = (reviews.date < pd.to_datetime(row.date)) & (reviews.restaurant_id == str(row.restaurant_id))
        
        # pre-inspection reviews
        pre_inspection_reviews = reviews[pre_inspection_mask]
        
        # join the text
        all_text = ' '.join(pre_inspection_reviews.text)

        # store in dictionarysssss
        reviews_dictionary[pid] = all_text
        
        if i % 2500 == 0:
            print '{} out of {}'.format(i, N)

    # return series in same order as the original data frame
    return pd.Series(reviews_dictionary)[label_df.index]

train_text = flatten_reviews(train_labels, reviews)
test_text = flatten_reviews(submission, reviews)


from sklearn.feature_extraction.text import TfidfVectorizer

# You can change the number of terms extracted by changing the
# max_features argument 
vec = TfidfVectorizer(stop_words='english',
                      max_features=250)

train_tfidf2 = vec.fit_transform(train_text)

df = pd.DataFrame(data=train_tfidf2.todense(), columns=vec.get_feature_names())

df.to_csv("featurestrain_df.csv", encoding='UTF-8')


train_tfidf3 = vec.fit_transform(test_text)

df = pd.DataFrame(data=train_tfidf3.todense(), columns=vec.get_feature_names())

df.to_csv("featurestest_df.csv", encoding='UTF-8')





