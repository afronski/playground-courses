
# This is a Python cell. You can run normal Python code here...
print 'The sum of 1 and 1 is {0}'.format(1+1)

# Here is another Python cell, this time with a variable (x) declaration and an if statement:
x = 42
if x > 40:
    print 'The sum of 1 and 2 is {0}'.format(1+2)

# This cell relies on x being defined already.
# If we didn't run the cells from part (1a) this code would fail.
print x * 2

# Import the regular expression library
import re
m = re.search('(?<=abc)def', 'abcdef')
m.group(0)

# Import the datetime library
import datetime
print 'This was last run on: {0}'.format(datetime.datetime.now())

# Display the type of the Spark Context sc
type(sc)

# List sc's attributes
dir(sc)

# Use help to obtain more detailed information
help(sc)

# After reading the help we've decided we want to use sc.version to see what version of Spark we are running
sc.version

# Help can be used on any Python object
help(map)

data = xrange(1, 10001)

# Data is just a normal Python list
# Obtain data's first element
data[0]

# We can check the size of the list using the len() function
len(data)

# Parallelize data using 8 partitions
# This operation is a transformation of data into an RDD
# Spark uses lazy evaluation, so no Spark jobs are run at this point
xrangeRDD = sc.parallelize(data, 8)

# Let's view help on parallelize
help(sc.parallelize)

# Let's see what type sc.parallelize() returned
print 'type of xrangeRDD: {0}'.format(type(xrangeRDD))

# How about if we use a range
dataRange = range(1, 10001)
rangeRDD = sc.parallelize(dataRange, 8)
print 'type of dataRangeRDD: {0}'.format(type(rangeRDD))

# Each RDD gets a unique ID
print 'xrangeRDD id: {0}'.format(xrangeRDD.id())
print 'rangeRDD id: {0}'.format(rangeRDD.id())

# We can name each newly created RDD using the setName() method
xrangeRDD.setName('My first RDD')

# Let's view the lineage (the set of transformations) of the RDD using toDebugString()
print xrangeRDD.toDebugString()

# Let's use help to see what methods we can call on this RDD
help(xrangeRDD)

# Let's see how many partitions the RDD will be split into by using the getNumPartitions()
xrangeRDD.getNumPartitions()

# Create sub function to subtract 1
def sub(value):
    """"Subtracts one from `value`.

    Args:
       value (int): A number.

    Returns:
        int: `value` minus one.
    """
    return (value - 1)

# Transform xrangeRDD through map transformation using sub function
# Because map is a transformation and Spark uses lazy evaluation, no jobs, stages,
# or tasks will be launched when we run this code.
subRDD = xrangeRDD.map(sub)

# Let's see the RDD transformation hierarchy
print subRDD.toDebugString()

# Let's collect the data
print subRDD.collect()

print xrangeRDD.count()
print subRDD.count()

# Define a function to filter a single value
def ten(value):
    """Return whether value is below ten.

    Args:
        value (int): A number.

    Returns:
        bool: Whether `value` is less than ten.
    """
    if (value < 10):
        return True
    else:
        return False
# The ten function could also be written concisely as: def ten(value): return value < 10

# Pass the function ten to the filter transformation
# Filter is a transformation so no tasks are run
filteredRDD = subRDD.filter(ten)

# View the results using collect()
# Collect is an action and triggers the filter transformation to run
print filteredRDD.collect()

lambdaRDD = subRDD.filter(lambda x: x < 10)
lambdaRDD.collect()

# Let's collect the even values less than 10
evenRDD = lambdaRDD.filter(lambda x: x % 2 == 0)
evenRDD.collect()

# Let's get the first element
print filteredRDD.first()
# The first 4
print filteredRDD.take(4)
# Note that it is ok to take more elements than the RDD has
print filteredRDD.take(12)

# Retrieve the three smallest elements
print filteredRDD.takeOrdered(3)
# Retrieve the five largest elements
print filteredRDD.top(5)

# Pass a lambda function to takeOrdered to reverse the order
filteredRDD.takeOrdered(4, lambda s: -s)

# Obtain Python's add function
from operator import add
# Efficiently sum the RDD using reduce
print filteredRDD.reduce(add)
# Sum using reduce with a lambda function
print filteredRDD.reduce(lambda a, b: a + b)
# Note that subtraction is not both associative and commutative
print filteredRDD.reduce(lambda a, b: a - b)
print filteredRDD.repartition(4).reduce(lambda a, b: a - b)
# While addition is
print filteredRDD.repartition(4).reduce(lambda a, b: a + b)

# takeSample reusing elements
print filteredRDD.takeSample(withReplacement=True, num=6)
# takeSample without reuse
print filteredRDD.takeSample(withReplacement=False, num=6)

# Set seed for predictability
print filteredRDD.takeSample(withReplacement=False, num=6, seed=500)
# Try reruning this cell and the cell above -- the results from this cell will remain constant
# Use ctrl-enter to run without moving to the next cell

# Create new base RDD to show countByValue
repetitiveRDD = sc.parallelize([1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 3, 3, 3, 4, 5, 4, 6])
print repetitiveRDD.countByValue()

# Let's create a new base RDD to work from
wordsList = ['cat', 'elephant', 'rat', 'rat', 'cat']
wordsRDD = sc.parallelize(wordsList, 4)

# Use map
singularAndPluralWordsRDDMap = wordsRDD.map(lambda x: (x, x + 's'))
# Use flatMap
singularAndPluralWordsRDD = wordsRDD.flatMap(lambda x: (x, x + 's'))

# View the results
print singularAndPluralWordsRDDMap.collect()
print singularAndPluralWordsRDD.collect()
# View the number of elements in the RDD
print singularAndPluralWordsRDDMap.count()
print singularAndPluralWordsRDD.count()

simpleRDD = sc.parallelize([2, 3, 4])
print simpleRDD.map(lambda x: range(1, x)).collect()
print simpleRDD.flatMap(lambda x: range(1, x)).collect()

pairRDD = sc.parallelize([('a', 1), ('a', 2), ('b', 1)])
# mapValues only used to improve format for printing
print pairRDD.groupByKey().mapValues(lambda x: list(x)).collect()

# Different ways to sum by key
print pairRDD.groupByKey().map(lambda (k, v): (k, sum(v))).collect()
# Using mapValues, which is recommended when they key doesn't change
print pairRDD.groupByKey().mapValues(lambda x: sum(x)).collect()
# reduceByKey is more efficient / scalable
print pairRDD.reduceByKey(add).collect()

# mapPartitions takes a function that takes an iterator and returns an iterator
print wordsRDD.collect()
itemsRDD = wordsRDD.mapPartitions(lambda iterator: [','.join(iterator)])
print itemsRDD.collect()

itemsByPartRDD = wordsRDD.mapPartitionsWithIndex(lambda index, iterator: [(index, list(iterator))])
# We can see that three of the (partitions) workers have one element and the fourth worker has two
# elements, although things may not bode well for the rat...
print itemsByPartRDD.collect()
# Rerun without returning a list (acts more like flatMap)
itemsByPartRDD = wordsRDD.mapPartitionsWithIndex(lambda index, iterator: (index, list(iterator)))
print itemsByPartRDD.collect()

# Name the RDD
filteredRDD.setName('My Filtered RDD')
# Cache the RDD
filteredRDD.cache()
# Is it cached
print filteredRDD.is_cached

# Note that toDebugString also provides storage information
print filteredRDD.toDebugString()

# If we are done with the RDD we can unpersist it so that its memory can be reclaimed
filteredRDD.unpersist()
# Storage level for a non cached RDD
print filteredRDD.getStorageLevel()
filteredRDD.cache()
# Storage level for a cached RDD
print filteredRDD.getStorageLevel()

def brokenTen(value):
    """Incorrect implementation of the ten function.

    Note:
        The `if` statement checks an undefined variable `val` instead of `value`.

    Args:
        value (int): A number.

    Returns:
        bool: Whether `value` is less than ten.

    Raises:
        NameError: The function references `val`, which is not available in the local or global
            namespace, so a `NameError` is raised.
    """
    if (val < 10):
        return True
    else:
        return False

brokenRDD = subRDD.filter(brokenTen)

# Now we'll see the error
brokenRDD.collect()

# Cleaner code through lambda use
subRDD.filter(lambda x: x < 10).collect()

# Even better by moving our chain of operators into a single line.
sc.parallelize(data).map(lambda y: y - 1).filter(lambda x: x < 10).collect()

# Final version
(sc
 .parallelize(data)
 .map(lambda y: y - 1)
 .filter(lambda x: x < 10)
 .collect())
