import glob
import json

from math import floor
from string import Template
from optparse import OptionParser

from svm import *
from svmutil import *

# Constants for coloring output.
GREEN_COLOR = '\033[92m'
RED_COLOR = '\033[91m'
BLUE_COLOR = '\033[94m'
END_COLOR = '\033[0m'

def read(filename):
	return svm_read_problem(filename)

def learn(labels, instances, index, options):
	# Creating SVM with linear kernel and specified parameters.
	# 	-s = 0  - C-SVM implementation
	#	-t = 0  - Linear kernel
	#	-c [X]  - Cost parameter
	#	-b 1    - Propability estimation.
	#	-n [Y]  - Nu parameter.
	template = Template("-s 0 -t 0 -b ${b} -c ${c} -n ${nu} ")

	return svm_train(
		labels[:index],
		instances[:index],
		template.safe_substitute(options))

def test(labels, instances, index, classifier, options):
	plabels, acc, val = svm_predict(labels[index:], instances[index:], classifier)
	(_, MSE, SCC) = evaluations(labels[index:], plabels)

	print("MSE:", MSE)
	print("SCC:", SCC)

	if options["verbose"]:	
		print("Labels:")
		print(json.dumps(labels, sort_keys=True, indent=4))
		print("Values:")
		print(json.dumps(val, sort_keys=True, indent=4))

parser = OptionParser()

parser.add_option("-c", "--cost", dest="c", help="adjust Cost parameter for SVM training")
parser.add_option("-n", "--nu", dest="nu", help="adjust Nu parameter for SVM training")
parser.add_option("-b", "--b", dest="b", help="whether train model for propability estimates")
parser.add_option("-r", "--ratio", dest="ratio", help="select ratio for splitting sets")
parser.add_option("-v", "--verbose", action="store_true", dest="verbose", help="display additional info")

options, args = parser.parse_args()

opts = {}

opts["c"]  	= options.c  		if options.c  		else 1
opts["nu"] 	= options.nu 		if options.nu 		else 0.5
opts["b"]  	= options.b  		if options.b  		else 0 
opts["ratio"]	= options.ratio		if options.ratio	else 0.5
opts["verbose"]	= True			if options.verbose	else False

files = glob.glob("../datasets/*.svm.txt")

for fileName in files:
	l, i = read(fileName)
	
	print("%s--- [Analyzing '%s'] ---%s" % (GREEN_COLOR, fileName, END_COLOR))
	print("%s--- Splitting with ratio %s ---%s" % (BLUE_COLOR, opts["ratio"], END_COLOR))

	index = floor(len(i) * (1 - float(opts["ratio"])))

	trained_classifier = learn(l, i, index, opts)
	test(l, i, index, trained_classifier, opts)
