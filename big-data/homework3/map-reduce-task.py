import glob
import re

from stopwords import *

punctuations = [ ",", ".", "'", '"', "!", "?", 
		 "-", "_", "(", ")", "&", "%", 
		 ":", ";", "`", "~", "[", "]" ]

# Server's part.

datasets = glob.glob("datasets/*")

def get_file_content(filename):
	f = open(filename)

	try:
		return f.read()
	finally:
		f.close()

source = dict((path, get_file_content(path)) for path in datasets)
output = open("output/results", "a")

# Client's part.

def removeStopWords(title):
	words = title.split(" ")

	for stopWord in allStopWords:
		words[:] = filter(lambda word: word != stopWord, words)

	return words

def removePunctuations(title):
	for mark in punctuations:
		title = title.replace(mark, "")

	return title

def trimWhiteSpaces(title):
	title = title.strip()
	title = re.sub(r"\s+", " ", title)

	return title

def mapfn(key, value):
	for line in value.splitlines():
		elements = line.split(":::")

		title = elements[2].lower()
		authors = elements[1].lower()

		title = removePunctuations(title)
		title = trimWhiteSpaces(title)
		words = removeStopWords(title)

		for author in authors.split("::"):
			yield author, words

def reducefn(author, wordsList):
	result = {}	
	
	for words in wordsList:
		for word in words:
			if result.has_key(word):
				result[word] += 1
			else:
				result[word] = 1

	output.write("%s ---> %s\n" % (author, str(result)))

	return result
