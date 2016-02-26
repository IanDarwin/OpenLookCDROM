#include "iostream.h"
#include "objects.h"

int main(int argc, char *argv[]) {
	Person ifd("Ian", "Darwin");
	Person bhd("Bascom", "Darwin");

	cout << ifd.getName() << endl;
	cout << bhd.getName() << endl;
}
