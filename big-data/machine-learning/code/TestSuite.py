# -*- coding: utf-8 -*-
#	Bachelor of Science Thesis				2o11/2o12
#	Author:									Wojciech Gawroński
#
#	Grammar Generator for internal searching language.
#	Main test suite.

import os
import fnmatch
import imp
import unittest
import warnings

# Switching off warnings which occures after dynamic module import.
warnings.filterwarnings("ignore")

# Test runner class.
class TestRunner:
	def __init__(self):
		self.path = "."
		self.pattern = "*Test.py"
		self.classNamePattern = "TestClass"

	# Gets python test files list based on pattern.
	def buildTestFilesList(self):
		matches = []

		for root, dirnames, filenames in os.walk(self.path):
			for filename in fnmatch.filter(filenames, self.pattern):
				matches.append(os.path.join(root, filename))

		return matches

	# Gets list of class names.
	def buildClassNamesList(self, testFilesList):
		classNames = []

		for testFilePath in testFilesList:
			moduleName = os.path.splitext(testFilePath)[0]
			module = imp.load_source(moduleName, testFilePath)

			for readClassName in dir(module):
				if readClassName == self.classNamePattern:
					classNames.append(module.TestClass)

		return classNames

	# Alias for loading test cases.
	def loadTestCase(self, className):
		return unittest.TestLoader().loadTestsFromTestCase(className)

	# Building whole test suite and run all tests.
	def buildTestSuite(self):
		testFilesList = self.buildTestFilesList()
		classNames = self.buildClassNamesList(testFilesList)
		
		return unittest.TestSuite([ self.loadTestCase(className) for className in classNames ])
	
	# Run test runner for command line.
	def runTextTestRunner(self):
		unittest.TextTestRunner().run(self.buildTestSuite())

if __name__ == '__main__':
	TestRunner().runTextTestRunner()