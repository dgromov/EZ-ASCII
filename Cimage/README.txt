In my program, it can read the image, and covert it to grey scale (There are some other ways to do it). The output is the array data, image information and image. 
I stored the depth in img[][] array (I'm not sure the data is correct or not. I will check it later). I think we can focus on the grey scale and intensity later. I will try to wrap the code to Ocaml first to see if it works.

Cimg Website: http://cimg.sourceforge.net/
Reference Manual: http://cimg.sourceforge.net/CImg_reference.pdf

1. In command line, use this to compile hello_world.cpp (In Mac):
g++ -o hello_world hello_world.cpp -02 -Wall -W -lm -ansi -pedantic -Dcimg_use_vt100 -I/usr/X11R6/include -L/usr/X11R6/lib -lpthread -lX11

2. To run
./hello_world