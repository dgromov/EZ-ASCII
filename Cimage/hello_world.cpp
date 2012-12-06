#include <iostream>
using namespace std;

#include "CImg.h"
using namespace cimg_library;

int main() {
	//decleation
	CImg<> image("lena.jpg");
	//grey scale
	image = image.get_norm(1) / (int)image.spectrum();

	int counter = 0;
	int width = image.width();
    int height = image.height();

    //initialize an array
    //int img[width][height] = {};

	int** img = new int*[width];
	for(int i = 0; i < width; ++i)
    	img[i] = new int[height];

	for (int y=0; y < height; y++) {
     for (int x=0; x < width; x++) {
       counter++;
       img[x][y]= (int)(image(x,y,1));
		cout << img[x][y] << endl;
       //cout << 	typeid(img[x][y]).name() << endl;
     }
}

	//print counter
	//cout << counter << endl;
	image.display("title");

	return 0;
}