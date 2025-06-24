import pandas as pd
import numpy as np  


libr = {'Self Help': 'Shelf_1',
         'Antique': 'Shelf_2',
         'Religion': 'Shelf_3',
         'Fiction': 'Shelf_4',
         'Technical': 'Shelf_5'}


Shelf_capacity = {'Shelf_1': 10,
                  'Shelf_2': 10,
                  'Shelf_3': 10,
                  'Shelf_4': 10,
                  'Shelf_5': 10}

num_stored_books = {'Shelf_1': 0,
                'Shelf_2': 0,
                'Shelf_3': 0,
                'Shelf_4': 0,
                'Shelf_5': 0}

book_index = {'Shelf_1': [],
             'Shelf_2': [],
             'Shelf_3': [],
             'Shelf_4': [],
             'Shelf_5': []}

finished_library = []


def build_library(df):
   name = pd.Series({})
   genre = pd.Series({})
   size = pd.Series({})
   shelf_number = pd.Series({})
   position = pd.Series({})
   unadded_books_cap = pd.Series({})
   unadded_books_genre = pd.Series({})
   for i in range(len(df['Name'])):
       if df['Genre'][i] in libr:
           if num_stored_books[libr[df['Genre'][i]]] < Shelf_capacity[libr[df['Genre'][i]]]:
               Shelf_capacity[libr[df['Genre'][i]]] -= df['Size'][i]
               num_stored_books[libr[df['Genre'][i]]] += df['Size'][i]
               book_index[libr[df['Genre'][i]]].append(df['Name'][i])
               name[i]= df['Name'][i]
               genre[i] = df['Genre'][i]
               size[i] = df['Size'][i]
               shelf_number[i] = libr[df['Genre'][i]]
               position[i] = book_index[libr[df['Genre'][i]]].index(df['Name'][i]) +1
           else:
               unadded_books_cap[i] = df['Name'][i]
       else:
              unadded_books_genre[i] = df['Name'][i]
   finished_library = pd.DataFrame({'Name': name, 'Genre': genre, 'Size': size, 'Shelf': shelf_number, 'index': position})
   print(finished_library, 
         '\nCould not add books:', unadded_books_cap.dropna().values, 'due to shelving capacity',
         '\nCould not add books:', unadded_books_genre.dropna().values, 'due to genre not being in library')



data = {
    'Name': ['The Charisma Myth', 'Eisenhower was my boss', 'Hayfoot Strawfoot', 'The daily Trading coach', 'What makes a man',
             'How to win friends and influence people'],
    'Genre': ['Self Help', 'Antique', 'Fiction', 'Technical', 'Self Help', 'Self Help'],
    'Size': [1, .5, .5, 1, 9, 1]
}

build_library(data)


            

