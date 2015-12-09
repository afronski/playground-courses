
labVersion = 'cs190_week2_word_count_v_1_0'

wordsList = ['cat', 'elephant', 'rat', 'rat', 'cat']
wordsRDD = sc.parallelize(wordsList, 4)
# Print out the type of wordsRDD
print type(wordsRDD)

# TODO: Replace <FILL IN> with appropriate code
def makePlural(word):
    """Adds an 's' to `word`.

    Note:
        This is a simple function that only adds an 's'.  No attempt is made to follow proper
        pluralization rules.

    Args:
        word (str): A string.

    Returns:
        str: A string with 's' added to it.
    """
    return <FILL IN>

print makePlural('cat')

# One way of completing the function
def makePlural(word):
    return word + 's'

print makePlural('cat')

# Load in the testing code and check to see if your answer is correct
# If incorrect it will report back '1 test failed' for each failed test
# Make sure to rerun any cell you change before trying the test again
from test_helper import Test
# TEST Pluralize and test (1b)
Test.assertEquals(makePlural('rat'), 'rats', 'incorrect result: makePlural does not add an s')

# TODO: Replace <FILL IN> with appropriate code
pluralRDD = wordsRDD.map(<FILL IN>)
print pluralRDD.collect()

# TEST Apply makePlural to the base RDD(1c)
Test.assertEquals(pluralRDD.collect(), ['cats', 'elephants', 'rats', 'rats', 'cats'],
                  'incorrect values for pluralRDD')

# TODO: Replace <FILL IN> with appropriate code
pluralLambdaRDD = wordsRDD.map(lambda <FILL IN>)
print pluralLambdaRDD.collect()

# TEST Pass a lambda function to map (1d)
Test.assertEquals(pluralLambdaRDD.collect(), ['cats', 'elephants', 'rats', 'rats', 'cats'],
                  'incorrect values for pluralLambdaRDD (1d)')

# TODO: Replace <FILL IN> with appropriate code
pluralLengths = (pluralRDD
                 <FILL IN>
                 .collect())
print pluralLengths

# TEST Length of each word (1e)
Test.assertEquals(pluralLengths, [4, 9, 4, 4, 4],
                  'incorrect values for pluralLengths')

# TODO: Replace <FILL IN> with appropriate code
wordPairs = wordsRDD.<FILL IN>
print wordPairs.collect()

# TEST Pair RDDs (1f)
Test.assertEquals(wordPairs.collect(),
                  [('cat', 1), ('elephant', 1), ('rat', 1), ('rat', 1), ('cat', 1)],
                  'incorrect value for wordPairs')

# TODO: Replace <FILL IN> with appropriate code
# Note that groupByKey requires no parameters
wordsGrouped = wordPairs.<FILL IN>
for key, value in wordsGrouped.collect():
    print '{0}: {1}'.format(key, list(value))

# TEST groupByKey() approach (2a)
Test.assertEquals(sorted(wordsGrouped.mapValues(lambda x: list(x)).collect()),
                  [('cat', [1, 1]), ('elephant', [1]), ('rat', [1, 1])],
                  'incorrect value for wordsGrouped')

# TODO: Replace <FILL IN> with appropriate code
wordCountsGrouped = wordsGrouped.<FILL IN>
print wordCountsGrouped.collect()

# TEST Use groupByKey() to obtain the counts (2b)
Test.assertEquals(sorted(wordCountsGrouped.collect()),
                  [('cat', 2), ('elephant', 1), ('rat', 2)],
                  'incorrect value for wordCountsGrouped')

# TODO: Replace <FILL IN> with appropriate code
# Note that reduceByKey takes in a function that accepts two values and returns a single value
wordCounts = wordPairs.reduceByKey(<FILL IN>)
print wordCounts.collect()

# TEST Counting using reduceByKey (2c)
Test.assertEquals(sorted(wordCounts.collect()), [('cat', 2), ('elephant', 1), ('rat', 2)],
                  'incorrect value for wordCounts')

# TODO: Replace <FILL IN> with appropriate code
wordCountsCollected = (wordsRDD
                       <FILL IN>
                       .collect())
print wordCountsCollected

# TEST All together (2d)
Test.assertEquals(sorted(wordCountsCollected), [('cat', 2), ('elephant', 1), ('rat', 2)],
                  'incorrect value for wordCountsCollected')

# TODO: Replace <FILL IN> with appropriate code
uniqueWords = <FILL IN>
print uniqueWords

# TEST Unique words (3a)
Test.assertEquals(uniqueWords, 3, 'incorrect count of uniqueWords')

# TODO: Replace <FILL IN> with appropriate code
from operator import add
totalCount = (wordCounts
              .map(<FILL IN>)
              .reduce(<FILL IN>))
average = totalCount / float(<FILL IN>)
print totalCount
print round(average, 2)

# TEST Mean using reduce (3b)
Test.assertEquals(round(average, 2), 1.67, 'incorrect value of average')

# TODO: Replace <FILL IN> with appropriate code
def wordCount(wordListRDD):
    """Creates a pair RDD with word counts from an RDD of words.

    Args:
        wordListRDD (RDD of str): An RDD consisting of words.

    Returns:
        RDD of (str, int): An RDD consisting of (word, count) tuples.
    """
    <FILL IN>
print wordCount(wordsRDD).collect()

# TEST wordCount function (4a)
Test.assertEquals(sorted(wordCount(wordsRDD).collect()),
                  [('cat', 2), ('elephant', 1), ('rat', 2)],
                  'incorrect definition for wordCount function')

# TODO: Replace <FILL IN> with appropriate code
import re
def removePunctuation(text):
    """Removes punctuation, changes to lower case, and strips leading and trailing spaces.

    Note:
        Only spaces, letters, and numbers should be retained.  Other characters should should be
        eliminated (e.g. it's becomes its).  Leading and trailing spaces should be removed after
        punctuation is removed.

    Args:
        text (str): A string.

    Returns:
        str: The cleaned up string.
    """
    <FILL IN>
print removePunctuation('Hi, you!')
print removePunctuation(' No under_score!')
print removePunctuation(' *      Remove punctuation then spaces  * ')

# TEST Capitalization and punctuation (4b)
Test.assertEquals(removePunctuation(" The Elephant's 4 cats. "),
                  'the elephants 4 cats',
                  'incorrect definition for removePunctuation function')

# Just run this code
import os.path
baseDir = os.path.join('data')
inputPath = os.path.join('cs100', 'lab1', 'shakespeare.txt')
fileName = os.path.join(baseDir, inputPath)

shakespeareRDD = (sc
                  .textFile(fileName, 8)
                  .map(removePunctuation))
print '\n'.join(shakespeareRDD
                .zipWithIndex()  # to (line, lineNum)
                .map(lambda (l, num): '{0}: {1}'.format(num, l))  # to 'lineNum: line'
                .take(15))

# TODO: Replace <FILL IN> with appropriate code
shakespeareWordsRDD = shakespeareRDD.<FILL_IN>
shakespeareWordCount = shakespeareWordsRDD.count()
print shakespeareWordsRDD.top(5)
print shakespeareWordCount

# TEST Words from lines (4d)
# This test allows for leading spaces to be removed either before or after
# punctuation is removed.
Test.assertTrue(shakespeareWordCount == 927631 or shakespeareWordCount == 928908,
                'incorrect value for shakespeareWordCount')
Test.assertEquals(shakespeareWordsRDD.top(5),
                  [u'zwaggerd', u'zounds', u'zounds', u'zounds', u'zounds'],
                  'incorrect value for shakespeareWordsRDD')

# TODO: Replace <FILL IN> with appropriate code
shakeWordsRDD = shakespeareWordsRDD.<FILL_IN>
shakeWordCount = shakeWordsRDD.count()
print shakeWordCount

# TEST Remove empty elements (4e)
Test.assertEquals(shakeWordCount, 882996, 'incorrect value for shakeWordCount')

# TODO: Replace <FILL IN> with appropriate code
top15WordsAndCounts = <FILL IN>
print '\n'.join(map(lambda (w, c): '{0}: {1}'.format(w, c), top15WordsAndCounts))

# TEST Count the words (4f)
Test.assertEquals(top15WordsAndCounts,
                  [(u'the', 27361), (u'and', 26028), (u'i', 20681), (u'to', 19150), (u'of', 17463),
                   (u'a', 14593), (u'you', 13615), (u'my', 12481), (u'in', 10956), (u'that', 10890),
                   (u'is', 9134), (u'not', 8497), (u'with', 7771), (u'me', 7769), (u'it', 7678)],
                  'incorrect value for top15WordsAndCounts')
