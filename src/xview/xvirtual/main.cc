//  1993 by M. Roth

#include "main.h"
#include "szene.h"
#include "view.h"
#include "zbuffer.h"
#include "shadow.h"
#include "shape.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <new.h>
#include "x.h"

extern char pfad[256];

// Alle resourcen wieder freigeben
void End()
{
  int c=0;

  ViewEnd();
  SzeneEnd();
  ShadowEnd();
  ZBufferEnd();
  ShapeEnd();
  cout << "XVirtual 1993 by Marcus Roth " << endl;
}

// Fehlerausgeben und Programm beenden
void Error(char *text)
{
  End();
  cout << text << endl;
  exit(16);
}

// Fehlerausgebe bei out of memory
void MemError()
{
  End();
  cout << "Not enough memory" << endl;
  exit(16);
}

void Interakt();
int Load(char *);
void Test();

// Hauptsteurung
int main(int argc,char *argv[])
{
  char filename[512] ={0};

  set_new_handler(MemError);
/*  if (argc<=1)
  {
    cout << "Bitte filenamen angeben" << endl;
    End();
    return 1;
  }*/
  ViewModeX();
  SzeneInit();
  ZBufferInit();
  ShadowInit();
  ShapeInit();
  if (argc>1)
    strcpy(filename,argv[1]);
  strcpy(pfad,filename);
  Load(filename);
  ViewInit();
  Interakt();
  End();
  return 0;
}

