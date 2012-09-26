import mincemeat
import glob

# Loading datasets.

datasets = glob.glob("datasets/*")

def get_file_content(filename):
	f = open(filename)

	try:
		return f.read()
	finally:
		f.close()

# Client's part.

def mapfn(key, value):
	for line in value.splitlines():
		for word in line.split():
			yield word.lower(), 1

def reducefn(key, value):
	return key, len(value)

# Server's part.
server = mincemeat.Server()

server.datasource = dict((path, get_file_content(path)) for path in datasets)
server.mapfn = mapfn
server.reducefn = reducefn

output = open("output/results", "w")

results = server.run_server(password='sample')
output.write(str(results))
