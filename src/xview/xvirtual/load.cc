//  1993 by M. Roth

#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <iostream.h>
#include <stdlib.h>
#include "main.h"
#include "szene.h"
#include "figuren.h"
#include "view.h"
 
#define NUM  0
#define TEXT 1

#define MAX_OBJ_NAME 14

int errorFlag=0;

struct Command
{
  int c;
  int parmCount;
  int parmTyp[30];
  char *parmText[30];
  float parmNum[30];
};

KoerperPtr koerperTab[30];
int koerperCount=-1;

AnimationPtr animationTab[10];
int animationCount=-1;

// Laenge eines Objektnamens testen
int TestName(char *name)
{
  if(strlen(name)>MAX_OBJ_NAME)
  {
    cout << "Objektname more than " << MAX_OBJ_NAME << " chars long." << endl;
    return 1;
  }
  return 0;
}

// Objekt beginnen
int CmdObjekt(Command *cmd)
{
  char *name = "";
  int laenge;
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = new Koerper(name);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  koerperCount++;
  koerperTab[koerperCount+1] = 0;
  return 0;
}

// Objekt beenden
int CmdEndObjekt(Command *cmd)
{
  if (koerperCount==0)
  {
    cout << "All Objects have already been closed." << endl;
    return 1;
  }
  if (cmd->parmCount>2) return 2;
  koerperCount--;
  return 0;
}

// Koordinate einfgen
int CmdPoint(Command *cmd)
{
  float x=0,y=0,z=0;
  int c=1;

  if (cmd->parmTyp[c] == NUM) x=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) y=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) z=cmd->parmNum[c++];
  koerperTab[koerperCount]->add(new Koordinate( Vektor(x,y,z) ));
  return 0;
}

// Flaeche einfgen
int CmdPolygon(Command *cmd)
{
  int nr;
  FlaechenPtr flaeche;

  if ((cmd->parmCount-cmd->c)<3)
  {
    cout << "A polygon must have 3 points minimum." << endl;
    return 1;
  }
  flaeche = new Flaeche( koerperTab[koerperCount] );
  koerperTab[koerperCount]->add(flaeche);
  for(cmd->c=1 ; cmd->c<cmd->parmCount ; cmd->c++)
  {
    if (cmd->parmTyp[cmd->c] == NUM)
    {
      nr=(int)cmd->parmNum[cmd->c]-1;
      if ((nr>=0) && (koerperTab[koerperCount]->KoordinatenLst()[nr]))
      {
	flaeche->add( new Eckpunkt( koerperTab[koerperCount]->KoordinatenLst()[nr]) );
      }
      else
      {
	cout << "Point " << nr+1 << " not found. " << endl;
	cout << "The first point has the number 1 !!" << endl;
	return 1;
      }
    }
  }
  if (flaeche->Initialize())
  {
    cout << "The points of a plane must not be places on a line." << endl;
    return 1;
  }
  return 0;
}

// Quader einfgen
int CmdQuader(Command *cmd)
{
  char *name = "";
  float x=1,y=1,z=1;
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  if (cmd->parmTyp[c] == NUM) x=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) y=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) z=cmd->parmNum[c++];
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = Quader(name,x,y,z);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  return 0;
}

// Pyramide einfgen
int CmdPyramide(Command *cmd)
{
  char *name = "";
  float b=1,h=1;
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  if (cmd->parmTyp[c] == NUM) b=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) h=cmd->parmNum[c++];
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = Pyramide(name,b,h);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  return 0;
}

// Prototyp fuer Load
int Load(char *);
// Text einfgen
int CmdText(Command *cmd)
{
  char *charset = "standard";
  char *text = " ";
  char buchstabe[2] = {0,0};
  char filename[25];
  int pos=0;
  char *name="";

  // objektname
  if ((cmd->parmTyp[cmd->c] == TEXT) && (*cmd->parmText[cmd->c]!='"'))
  {
    name=cmd->parmText[cmd->c++];
  }
  // Text
  if ((cmd->parmTyp[cmd->c] == TEXT) && (*cmd->parmText[cmd->c]=='"'))
  {
    text=cmd->parmText[cmd->c++];
  }
  // Anderen Zeichensatz verwenden
  if ((cmd->parmTyp[cmd->c] == TEXT) && (!(strcmp(cmd->parmText[cmd->c],"charset"))))
  {
    cmd->c++;
    if (cmd->parmTyp[cmd->c] == TEXT) charset=cmd->parmText[cmd->c++]; else return 2;
  }
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = new Koerper(name);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  koerperCount++;
  text++;
  while ( (*text!='"') && (*text!=0) )
  {
    if (*text != ' ')
    {
      buchstabe[0]=*text;
      strcpy(filename,charset);
      strcat(filename,"/");
      strcat(filename,buchstabe);
      if (Load(filename)) return 1;
      koerperTab[koerperCount+1]->Verschiebe( Vektor(pos,0,0) );
    }
    pos+=10;
    text++;
  }
  koerperCount--;
  return 0;
}

// Punkt fr Rotationsk”rper einfgen
int CmdRotPoint(Command *cmd)
{
  float x=1,z=1;
  int c=1;

  if (cmd->parmTyp[c] == NUM) x=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) z=cmd->parmNum[c++];
  FigurPointAdd(x,z);
  return 0;
}

// Rotationsbogen erstellen
int CmdRotArc(Command *cmd)
{
  float x=1,y=1,w=180,s=2;
  int c=1;

  if (cmd->parmTyp[c] == NUM) x=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) y=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) w=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) s=cmd->parmNum[c++];
  RotArc(x,y,w,s);
  return 0;
}

// Rotationsk”rper einfgen
int CmdRotObjekt(Command *cmd)
{
  char *name = "";
  float s=4;
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  if (cmd->parmTyp[c] == NUM) s=cmd->parmNum[c++];
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = RotKoerper(name,(int)s);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  return 0;
}

// Punkt fr Sweep-Koerper einfgen
int CmdSweepPoint(Command *cmd)
{
  float x=1,z=1;
  int c=1;

  if (cmd->parmTyp[c] == NUM) x=cmd->parmNum[c++];
  if (cmd->parmTyp[c] == NUM) z=cmd->parmNum[c++];
  FigurPointAdd(x,z);
  return 0;
}

// Sweepbogen erstellen
int CmdSweepArc(Command *cmd)
{
  return CmdRotArc(cmd);
}

// Sweep-K”rper einfgen
int CmdSweepObjekt(Command *cmd)
{
  char *name = "";
  float hoehe=-1;
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  if (cmd->parmTyp[c] == NUM) hoehe=cmd->parmNum[c++]; else return 2;
  if (hoehe<0) return 2;
  if (TestName(name)) return 1;
  koerperTab[koerperCount+1] = SweepKoerper(name,hoehe);
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  return 0;
}

// Farbe ausw„hlen
int CmdColor(Command *cmd)
{
  char *name = "weiss";
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++];
  ViewSetColor(name);
  return 0;
}

// Objekt kopieren
int CmdCopy(Command *cmd)
{
  char *nameVon = "";
  char *nameNach = 0;
  int c=1;
  KoerperPtr		koer;

  if (cmd->parmTyp[c] == TEXT) nameVon=cmd->parmText[c++];
  if (cmd->parmTyp[c] == TEXT) nameNach=cmd->parmText[c++];
  koerperTab[koerperCount+1] = new Koerper;
  koerperTab[koerperCount]->add(koerperTab[koerperCount+1]);
  koer=SzeneGetKoerper(nameVon);
  if (!(koer))
  {
    cout << "Sourceobject not found : " << nameVon << endl;
    return 1;
  }
  *koerperTab[koerperCount+1] = *koer;
  if (nameNach)
  {
    if (TestName(nameNach))
      return 1;
    koerperTab[koerperCount+1]->Rename(nameNach);
  }
  return 0;
}

// Objektname suchen
KoerperPtr SearchName(char *name)
{
  KoerperPtr 	koer=0;

  if (koerperTab[koerperCount+1]!=0)
    koer = koerperTab[koerperCount+1]->SucheKoerper(name);
  if (koer==0)
    koer = koerperTab[koerperCount]->SucheKoerper(name);
  return koer;
}

// Objekt selektieren
int CmdSelect(Command *cmd)
{
  char *name = "";
  int c=1;

  if (cmd->parmTyp[c] == TEXT) name=cmd->parmText[c++]; else return 2;
  koerperTab[koerperCount+1]=SearchName(name);
  if (!(koerperTab[koerperCount+1]))
  {
    cout << "Object not found : " << name << endl;
    return 1;
  }
  return 0;
}

// Grafikmodus sezten
int CmdMode(Command *cmd)
{
  int mode = ANIMATE;
  int c;

  while (cmd->parmTyp[cmd->c] == TEXT)
  {
    c=cmd->c;
    if (!(strcmp("wire",cmd->parmText[cmd->c])))
      { cmd->c++; mode |= WIRE; }
    if (!(strcmp("solid",cmd->parmText[cmd->c])))
      { cmd->c++; mode |= SOLID; }
    if (!(strcmp("shadow",cmd->parmText[cmd->c])))
      { cmd->c++; mode |= SHADOW; }
    if (c==cmd->c)
      return 2;
  }
  if (mode==0)
  {
    cout << "Unknowen mode." << endl;
    return 1;
  }
  if (animationCount==-1)
    SzeneDrawMode(mode);
  else
    animationTab[ animationCount ]->add(new Aktion(mode,0,0,ANIM_MODE));
  return 0;
}

// Programm beenden
int CmdExit(Command *cmd)
{
  if (cmd->c<cmd->parmCount) return 2;
  if (animationCount==-1)
    Error("Programm durch EXIT beendet.");
  else
    animationTab[ animationCount ]->add(new Aktion(0,0,0,ANIM_EXIT));
  return 0;
}

//-----------------------------------------------------------------
//  Animationsbefehle
//-----------------------------------------------------------------

// Prototyp fuer Befehl suchen
int (*SearchCommand(char *name))(Command *);
// Animationsstruktur einfgen
int CmdAnim(Command *cmd)
{
  float 		sec=1;
  int 			flag=0;
  long 			loopcount;
  int 			rc=0;
  int 			(*command)(Command *);
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (cmd->parmTyp[cmd->c] == NUM) sec=cmd->parmNum[cmd->c++]; else return 2;
  // loop abfragen
  if ((cmd->parmTyp[cmd->c] == TEXT) && (!(strcmp(cmd->parmText[cmd->c],"loop"))))
  {
    flag|=ANIM_LOOP;
    cmd->c++;
    loopcount=0;
    // anzahl der Schleifendurchl„ufe
    if (cmd->parmTyp[cmd->c] == NUM) loopcount=(int)cmd->parmNum[cmd->c++];
  }
  // Speedup abfragen
  if ((cmd->parmTyp[cmd->c] == TEXT) && (!(strcmp(cmd->parmText[cmd->c],"speedup"))))
  {
    flag|=ANIM_SPEEDUP;
    cmd->c++;
  }
  // Slowdown abfragen
  if ((cmd->parmTyp[cmd->c] == TEXT) && (!(strcmp(cmd->parmText[cmd->c],"slowdown"))))
  {
    flag|=ANIM_SLOWDOWN;
    cmd->c++;
  }
  animationCount++;
  animationTab[ animationCount ] = new Animation(sec,flag,loopcount,koerper);
  if(animationCount==0)
  {
    koerperTab[koerperCount+1]->add(animationTab[animationCount]);
  }
  else
  {
    animationTab[animationCount-1]->add(animationTab[animationCount]);
  }
  if (cmd->parmTyp[cmd->c] == TEXT)
  {
     if (strcmp("wait",cmd->parmText[cmd->c]))
     {
       command=SearchCommand(cmd->parmText[cmd->c++]);
       if (!(command))
       {
	 cout << "Command not found or not valid." << endl;
	 return 1;
       }
       rc = command(cmd);
     }
     animationCount--;
     return rc;
  }
  return 0;
}

// Animationstruktur beenden
int CmdEndAnim(Command *cmd)
{
  if (animationCount==-1)
  {
    cout << "Animationens have already been closed." << endl;
  }
  if (cmd->parmCount>1)
  {
    cout << "To many parameters." << endl;
    return 1;
  }
  animationCount--;
  return 0;
}

// Startzeit fuer eine Animation angeben
int CmdAnimSet(Command *cmd)
{
  AnimationPtr		animPtr;
  float			start=0;
  int 			c=1;

  if (cmd->parmTyp[c] == NUM) start=cmd->parmNum[c++];
  koerperTab[koerperCount+1]->AnimSetStartTime(start);
  return 0;
}

//-----------------------------------------------------------------
//  Manipulationsbefehle
//-----------------------------------------------------------------

// Aktuellen Koerper verschieben
int CmdMove(Command *cmd)
{
  float 		x=0,y=0,z=0;
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (cmd->parmTyp[cmd->c] == NUM) x=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) y=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) z=cmd->parmNum[cmd->c++];
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Verschiebe( Vektor(x,y,z) );
  else
    animationTab[ animationCount ]->add(new Aktion(x,y,z,ANIM_MOVE,koerper));
  return 0;
}

// Aktuellen Koerper rotieren
int CmdRotate(Command *cmd)
{
  float 		x=0,y=0,z=0;
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (cmd->parmTyp[cmd->c] == NUM) x=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) y=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) z=cmd->parmNum[cmd->c++];
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Rotiere( Matrix(x,y,z) );
  else
    animationTab[ animationCount ]->add(new Aktion(x,y,z,ANIM_ROTATE,koerper));
  return 0;
}

// Koerper skalieren
int CmdScale(Command *cmd)
{
  float 		x=0,y=0,z=0;
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (cmd->parmTyp[cmd->c] == NUM) x=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) y=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) z=cmd->parmNum[cmd->c++];
  if ((y==0) && (z==0) && (x!=0))
    z=y=x;
  if ((x<0) || (y<0) || (z<0))
  {
    cout << "Scale must be greater than 0." << endl;
    return 1;
  }
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Scale( x,y,z );
  else
    animationTab[ animationCount ]->add(new Aktion(x,y,z,ANIM_SCALE,koerper));
  return 0;
}

// Koerper zentrieren
int CmdCentre(Command *cmd)
{
  float 		x=0,y=0,z=0;
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (cmd->parmTyp[cmd->c] == NUM) x=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) y=cmd->parmNum[cmd->c++];
  if (cmd->parmTyp[cmd->c] == NUM) z=cmd->parmNum[cmd->c++];
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Centre( Vektor(x,y,z) );
  else
    animationTab[ animationCount ]->add(new Aktion(x,y,z,ANIM_CENTRE,koerper));
  return 0;
}

// Objekt verstecken
int CmdHide(Command *cmd)
{
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Hide();
  else
    animationTab[ animationCount ]->add(new Aktion(0,0,0,ANIM_HIDE,koerper));
  return 0;
}

// Objekt anzeigen
int CmdShow(Command *cmd)
{
  KoerperPtr		koerper=koerperTab[koerperCount+1];

  if (!(koerper))
  {
    cout << "No current object." << endl;
    return 1;
  }
  if (cmd->parmTyp[cmd->c] == TEXT) koerper=koerper->SucheKoerper(cmd->parmText[cmd->c++]);
  if (!(koerper))
  {
    cout << "Object not found." << endl;
    return 1;
  }
  if (animationCount==-1)
    koerper->Show();
  else
    animationTab[ animationCount ]->add(new Aktion(0,0,0,ANIM_SHOW,koerper));
  return 0;
}

//---------------------------------------------------------------------

// Zeile einlesen
int ReadLine(FILE *handle,char *buffer1,char *buffer2)
{
  int flag=0;

  if (feof(handle))
    return 0;
  *buffer1=fgetc(handle);
  while (!(feof(handle)))
  {
    flag=1;
    if ((*buffer1 == 10) || (*buffer1 == 0)) 
    {
      *buffer1=0;
      *buffer2=0;
      return 1;
    }
    *buffer2=*buffer1;
    if (*buffer1 == ';')
      *buffer1=0;
    buffer1++;
    buffer2++;
    *buffer1=fgetc(handle);
  }
  return flag;
}

// Commando aufbereiten
void ParseCmd(char *ptr,Command *cmd)
{
  char *endPtr;
  int c;

  cmd->parmCount=0;
  while (*ptr!=0)
  {
    while ((*ptr == 32) || (*ptr == 9)) ptr++;
    if (*ptr == 0) break;
    if (
	((*ptr >= '0') && (*ptr <= '9')) ||
	(*ptr == '+') ||
	(*ptr == '-')
       )
    {
      cmd->parmTyp[ cmd->parmCount ] = NUM;
      cmd->parmNum[ cmd->parmCount ] = strtod(ptr, &endPtr);
      ptr=endPtr;
      while ((*ptr != 32) && (*ptr != 9) && (*ptr != 0)) ptr++;
    }
    else
    {
      cmd->parmTyp[ cmd->parmCount ] = TEXT;
      cmd->parmText[ cmd->parmCount ] = ptr;
      if (*ptr=='"')
      {
	ptr++;
	while ((*ptr != '"') && (*ptr != 0)) ptr++;
	if(*ptr=='"') ptr++;
      }
      else
	while ((*ptr != 32) && (*ptr != 9) && (*ptr != 0)) ptr++;
    }
    if (*ptr!=0)
    {
      *ptr=0;
      ptr++;
    }
    cmd->parmCount++;
  }
  cmd->parmTyp[ cmd->parmCount ] = -1;
  cmd->c=0;
}

// Befehlstabelle
struct Befehl
{
  char *name;
  int (*funktion)(Command *);
} befehl[35] =
{
  "point",&CmdPoint,
  "polygon",&CmdPolygon,
  "stone",&CmdQuader,
  "pyramide",&CmdPyramide,
  "rotpoint",&CmdRotPoint,
  "rotarc",&CmdRotArc,
  "rotobject",&CmdRotObjekt,
  "sweeppoint",&CmdSweepPoint,
  "sweeparc",&CmdSweepArc,
  "sweepobject",&CmdSweepObjekt,
  "color",&CmdColor,
  "object",&CmdObjekt,
  "endobject",&CmdEndObjekt,
  "select",&CmdSelect,
  "copy",&CmdCopy,
  "anim",&CmdAnim,
  "endanim",&CmdEndAnim,
  "animset",&CmdAnimSet,
  "move",&CmdMove,
  "rotate",&CmdRotate,
  "scale",&CmdScale,
  "centre",&CmdCentre,
  "show",&CmdShow,
  "hide",&CmdHide,
  "text",&CmdText,
  "mode",&CmdMode,
  "exit",&CmdExit,
  0,0
};

struct Befehl animBefehl[15] =
{
  "move",&CmdMove,
  "rotate",&CmdRotate,
  "scale",&CmdScale,
  "centre",&CmdCentre,
  "show",&CmdShow,
  "hide",&CmdHide,
  "anim",&CmdAnim,
  "endanim",&CmdEndAnim,
  "mode",&CmdMode,
  "exit",&CmdExit,
  0,0
};

// Befehl suchen
int (*SearchCommand(char *name))(Command *)
{
   int c;

   c=0;
   if (animationCount==-1)
   {
     while ((befehl[c].name!=0) && (strcmp(name,befehl[c].name))) c++;
   }
   else
   {
     while ((animBefehl[c].name!=0) && (strcmp(name,animBefehl[c].name))) c++;
   }
   if (animationCount==-1)
      return befehl[c].funktion;
   else
      return animBefehl[c].funktion;
}

// Parameterfile laden
int Load(char *file)
{
   FILE *handle;
   int anz;
   struct Command cmd;
   char lineWork[256];
   char lineSave[256];
   char path[256];
   int line=0;
   int rc;
   int (*command)(Command *);
   int SavAnim,SavKoerper;
   int flag=0;

   // Basiskoerper laden
   if (koerperCount==-1)
   {
     koerperTab[0] = SzeneGetKoerper("szene");
     koerperTab[1] = 0;
     ViewSetColor("white");
     SzeneSetLight(-45,25);
     SzeneDrawMode(WIRE|ANIMATE);
     flag=1;
     if (file[0]==0) return 0;
     koerperCount++;
   }
   // Animations und Objektzaehler merken
   SavAnim=animationCount;
   SavKoerper=koerperCount;
   // File oeffnen
   strcpy(path,file);
   if ((handle = fopen(path,"r")) == 0)
   {
     strcat(path,".3d");
     if ((handle = fopen(path,"r")) == 0)
       {
	 strcpy(path,"3d/");
	 strcat(path,file);
	 strcat(path,".3d");
	 if ((handle = fopen(path, "r")) == 0)
	   {
	     cout << "File or command not found : " << path << endl;
	     return 1;
	   }
       }
   }
   // Alle Zeilen verarbeiten
   while ( ReadLine(handle,lineWork,lineSave) )
   {
     line++;
     ParseCmd(lineWork,&cmd);
     if( cmd.parmCount>0 )
     {
       rc=0;
       if( cmd.parmTyp[0] != TEXT)
       {
	 cout << "Row must start with a command." << endl;
	 rc=1;
       }
       else
       {
	 command=SearchCommand(cmd.parmText[0]);
	 cmd.c++;
	 if (command==0)
	    rc = Load(cmd.parmText[0]);
	 else
	    rc = command(&cmd);
       }
       if (rc)
       {
	 if (rc==2)
	   cout << "Syntax error." << endl;
	 cout << "Error in file " << path << endl;
	 cout << line << ": " << lineSave << endl;
	 fclose(handle);
	 return 1;
       }
     }
   }
   fclose(handle);
   if (SavAnim!=animationCount)
   {
     cout << "Not enough or to mutch endanim." << endl;
     return 1;
   }
   if (SavKoerper!=koerperCount)
   {
     cout << "Not enough or to mutch endobject." << endl;
     return 1;
   }
   if (flag)
     koerperCount--;
   return 0;
}

