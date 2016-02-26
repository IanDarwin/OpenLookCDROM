//  1993 by M. Roth

KoerperPtr Quader(char *,float,float,float);
KoerperPtr Pyramide(char *,float,float);
KoerperPtr Stern(char *,float,float);
void FigurPointAdd(float x,float y,Vektor *v=0);
void RotArc(float x,float y,float winkel,float segmente);
KoerperPtr RotKoerper(char *,int Segmente);
KoerperPtr SweepKoerper(char *,float hoehe);