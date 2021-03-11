#include <signal.h>

int sigvec(Signal, Invec, Outvec)
int Signal;
struct sigvec *Invec, *Outvec; {
    return((int)sigvector(Signal, Invec, Outvec));
}
