import requests
import re
import os
import csv


###############################################################################
# First, let's write some functions to get the data from the web.
###############################################################################

# define the URL of the main page of the bolha cats listing
cats_frontpage_url = (
    'http://www.bolha.com/zivali/male-zivali/'
    'macke/?sort=0&page=1'
)
# the directory to which we save our data
cat_directory = '2-zajem-podatkov/exercises/data_from_bolha'
# the filename we use to save the frontpage
frontpage_filename = 'cats1.html'
# the filename for the CSV file for the extracted data
csv_filename = 'kittens.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # some code here that may raise an exception
        r = requests.get(url)
        # some more code that won't be run if the exception occured
    except requests.exceptions.ConnectionError:
        # some error handling / recovery code here
        # we may just display an informative message and quit
        print('download failed!')
        return None
    # continue with the non-exceptional code
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Define a function that downloads the frontpage and saves it to a file.


def save_frontpage(url, directory, filename):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    text = download_url_to_string(url)
    return save_string_to_file(text, directory, filename)

###############################################################################
# Now that we have some data, we can think about processing it.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file_in:
        return file_in.read()

# Define a function that takes a webpage as a string and splits it into
# segments such that each segment corresponds to one advertisement. This
# function will use a regular expression that delimits the beginning and end of
# each ad. Return the list of strings.
# Hint: To build this reg-ex, you can use your text editor's regex search
# functionality.


def page_to_ads(webpage):
    '''Split "page" to a list of advertisement blocks.'''
    # webpage = read_file_to_string(directory, filename)
    # webpage is a string, which is read from the file 'filename'
    # (from the directory 'directory').
    # Individual blocks of ads are separated by the following regex:
    sample1 = re.compile(
        r'<div class="coloumn image">'
    )
    # We exclude the part from the last ad segment to the end of the page.
    sample2 = re.compile(
        r'<div class="clear">&nbsp;</div>'
    )
    list = re.split(sample1, webpage)
    last = list[-1]
    last = re.split(sample2, last)
    list[-1] = last[0]
    # We also exclude the segment from the begining of the page to the
    # first block of ad.
    return list[1:]

# Define a function that takes a string corresponding to the block of one
# advertisement and extracts from it the following data: Name, price, and
# the description as displayed on the page.


def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    sample = re.compile(
        r'<h3><a title="(?P<name>.+?)" href=".*?'
        r'</a></h3>' r'\n\n' r'\s+(?P<description>.*?)\s+<div class=".*?'
        r'<div class="price">(<span>|)(?P<price>.*?)(</span>|)</div>.*?'
        r'<a href="javascript:void\(0\);" data-id="(?P<id>\d+)" title=.*?',
        re.DOTALL
    )
    for expression in sample.finditer(block):
        dict = expression.groupdict()
    return dict

# Write a function that reads a page from a file and returns the list of
# dictionaries containing the information for each ad on that page.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    webpage = read_file_to_string(directory, filename)
    list_of_ads = page_to_ads(webpage)
    list_of_dicts = []
    for i in range(0, len(list_of_ads)):
        list_of_dicts.append(get_dict_from_ad_block(list_of_ads[i]))
    return list_of_dicts

###############################################################################
# We processed the data, now let's save it for later.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Write a function that takes a non-empty list of cat advertisement
# dictionaries and writes it to a csv file. The [fieldnames] can be read off
# the dictionary.


def write_cat_ads_to_csv(list_of_dicts_ads, directory, filename):
    '''Write a CSV file containing one ad from "ads" on each row.'''
    fieldnames = ['id', 'name', 'description', 'price']
    write_csv(fieldnames, list_of_dicts_ads, directory, filename)
    return None
