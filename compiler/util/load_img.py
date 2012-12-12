import Image 
import sys 
import os 

filename = sys.argv[1]
granularity = int(sys.argv[2])

im = Image.open(filename)

outfile = open (filename + '.i' , 'w')

im = im.convert('L')
im.thumbnail ((100, 100))
x, y = im.size 

result = ""
for i in xrange(x): 
	line = ""
	for k in xrange(y): 
		line += str(im.getpixel((k,i)) / (256 / granularity)) + " "
	result += line[:-1]  # Skip extra space 
	result += "\n"

outfile.write (result);
outfile.close(); 

