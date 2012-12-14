import Image 
import sys 
import os 

filepath = sys.argv[1]
granularity = int(sys.argv[2])

direc, file_name = os.path.split(filepath) 

print "PY DEBUG: filepath" 
im = Image.open(filepath)
outfile = open('../tmp/' + file_name + '.i' , 'w')

im = im.convert('L')
im.thumbnail ((100, 100))
x, y = im.size 

print "PY DEBUG: x -> {0}, y -> {1}".format(x,y) 
result = ""
for k in xrange(y): 
	line = ""
	for i in xrange(x): 
		line += str(im.getpixel((i,k)) / (256 / granularity)) + " "
	result += line[:-1]  # Skip extra space 
	result += "\n"

outfile.write(result)
outfile.close() 