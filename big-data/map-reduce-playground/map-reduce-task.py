import glob

# Server's part.

datasets = glob.glob("datasets/*")

def get_file_content(filename):
	f = open(filename)

	try:
		return f.read()
	finally:
		f.close()

source = dict((path, get_file_content(path)) for path in datasets)

output = open("output/map_reduce_results", "w")

def final(key, value):
	print key, value
	output.write(str((key, value)))

# Client's part.

def mapfn(key, value):
	for line in value.splitlines():
		for word in line.split():
			yield word.lower(), 1

def reducefn(key, value):
	return key, len(value)
