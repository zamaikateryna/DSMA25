import os
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.feature_extraction.text import TfidfVectorizer
import spacy
import spacy.cli


def load_and_prepare_data():
    # Load reviews and business data
    reviews = pd.read_csv('data/review.csv')
    business = pd.read_csv('data/business.csv')

    # Clean star ratings
    reviews_clean = reviews.dropna(subset=['stars'])
    reviews_clean['stars'] = reviews_clean['stars'].astype(int)
    reviews_clean = reviews_clean[reviews_clean['stars'].isin([1, 2, 3, 4, 5])]

    # Merge price range information by foreign key business_id
    merged = reviews_clean.merge(
        business[['business_id', 'restaurantspricerange']],
        on='business_id',
        how='left'
    )
    merged = merged.dropna(subset=['restaurantspricerange'])
    merged['restaurantspricerange'] = merged['restaurantspricerange'].astype(int)

    return merged


def prepare_lemmatizer():
    # Ensure SpaCy model is downloaded and load it
    # Using small english model
    spacy.cli.download("en_core_web_sm")
    return spacy.load("en_core_web_sm", disable=["ner", "parser"])


def lemmatize_texts(nlp, texts):
    # Lowercase, lemmatize, and remove stop words and non-alpha tokens
    return texts.dropna().astype(str).apply(
        lambda text: " ".join(
            token.lemma_ for token in nlp(text.lower())
            if token.is_alpha and not token.is_stop
        )
    ).tolist()


def compute_top_tfidf(docs, top_n=20):
    #load vectorizer
    vectorizer = TfidfVectorizer()
    #calculate tf-idf for all terms (features) of the reviews
    X = vectorizer.fit_transform(docs)
    feature_array = vectorizer.get_feature_names_out()
    #extract tf-idf scores for every term
    tfidf_scores = X.mean(axis=0).A1
    # reverse sorted list and get only the 20 highest terms
    top_indices = tfidf_scores.argsort()[::-1][:top_n]
    # return list of tuples for plotting
    return [(feature_array[i], tfidf_scores[i]) for i in top_indices]


def plot_top_terms(terms_scores, title, save_path):

    #unpack tuples into two lists to create to traces for the plot
    terms, scores = zip(*terms_scores)

    #define optical properties and pass our traces; save figure
    plt.figure(figsize=(8, 5))
    plt.barh(terms, scores, color='cornflowerblue')
    plt.xlabel("TF-IDF Score")
    plt.title(title)
    plt.gca().invert_yaxis()
    plt.tight_layout()
    plt.savefig(save_path)
    plt.close()

def extract_term_score(docs, term):
    #load vectorizer
    vectorizer = TfidfVectorizer()
    X = vectorizer.fit_transform(docs)
    #calculate tf-idf for all terms (features) of the reviews
    feature_names = vectorizer.get_feature_names_out()
    #extract index for term and return respective score
    term_index = list(feature_names).index(term)
    tfidf_scores = X.mean(axis=0).A1

    return tfidf_scores[term_index]

if __name__ == "__main__":

    df = load_and_prepare_data()
    nlp = prepare_lemmatizer()

    # Group and analyze by star ratings
    os.makedirs("figures_stars", exist_ok=True) # create folder if it does not exist
    for stars in [1, 2, 3, 4, 5]:
        group = df[df['stars'] == stars]
        docs = lemmatize_texts(nlp, group['text'])
        top_terms = compute_top_tfidf(docs)
        plot_top_terms(top_terms, f"Top TF-IDF Terms – {stars} Stars", f"figures_stars/tfidf_stars_{stars}.png")

    # Group and analyze by price range
    os.makedirs("figures_price", exist_ok=True) # create folder if it does not exist
    for price in [1, 2, 3, 4]:
        group = df[df['restaurantspricerange'] == price]
        docs = lemmatize_texts(nlp, group['text'])
        top_terms = compute_top_tfidf(docs)
        plot_top_terms(top_terms, f"Top TF-IDF Terms – Price Range {price}", f"figures_price/tfidf_price_{price}.png")


    # Calculate TF-IDF of the term "service" in both data sets
    service_scores_stars = []
    service_scores_price = []
    
    # extract core in the stars subset
    for stars in [1, 2, 3, 4, 5]:
        group = df[df['stars'] == stars]
        docs = lemmatize_texts(nlp, group['text'])
        score = extract_term_score(docs, "service")
        service_scores_stars.append((stars, score))

    # extract core in the price range subset
    for price in [1, 2, 3, 4]:
        group = df[df['restaurantspricerange'] == price]
        docs = lemmatize_texts(nlp, group['text'])
        score = extract_term_score(docs, "service")
        service_scores_price.append((price, score))

    # Prepare data traces
    stars_x, stars_y = zip(*service_scores_stars)
    price_x, price_y = zip(*service_scores_price)
    
    #define optical properties and pass our traces; save figure
    plt.figure(figsize=(10, 5))
    plt.plot(stars_x, stars_y, marker='o', label="By Stars")
    plt.plot(price_x, price_y, marker='o', label="By Price Range")
    plt.xlabel("Category (Stars / Price Range)")
    plt.ylabel('TF-IDF Score of "service"')
    plt.title('TF-IDF Score of "service" by Stars and Price Range')
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    os.makedirs("figures_service", exist_ok=True) # create folder if it does not exist
    plt.savefig("figures_service/service_tfidf_trend.png")
    plt.close()