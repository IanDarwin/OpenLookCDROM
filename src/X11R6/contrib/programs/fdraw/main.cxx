/*
 * main.cxx
 */

#include <X11/Fresco/fresco.h>
#include "figviewer.h"
#include "fdraw.h"
#include "globals.h"

static Option options[] = {
    { nil }
};

int main(int argc, char** argv) {
    Fresco* f = Fresco_open("Fdraw", argc, argv, options);
    global_init(f);
    Viewer_var fdraw = new FDraw(f);
    f->run(fdraw, fdraw);
    return 0;
}
