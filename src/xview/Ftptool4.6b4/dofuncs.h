
#pragma ident   "@(#)dofuncs.h 1.3     93/05/25"

#define	DOCONNECT		1
#define	DOGET			2
#define	DOPUT			3
#define	DOLOCALVIEW		4
#define	DOREMOTEVIEW		5
#define	DOUNCOMPRESS		6
#define	DOEXTRACT		7
#define	DOCOMPRESS		8
#define	DOGETTARFILENAME	9
#define	DOTAR			10
#define	DOREMOTECD		11
#define	DOREMOTECDFORCE		12
#define	DOBATCHGET		13
#define	DOBATCHPUT		14
#define	DOSCHEDULE		15
#define	DODIR			16
#define	DOLS			17
#define	DOREMOTEDOUBLECLICK	18
#define	DOQUIT			(-1)

#ifdef USE_PROTOTYPES

int doconnect(void);
int openhost(char *ftphost, char *login, char *password,
	char *account, short port);
void doget(void);
void doput(void);
void dofileop(int which);
void doview(int which);
void create_tar_file(void);
void doremotecd(int force);

#else

int doconnect();
int openhost();
void doget();
void doput();
void dofileop();
void doview();
void create_tar_file();
void doremotecd();

#endif
