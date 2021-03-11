/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wpi/RCS/wpilib.c,v 1.35 1993/06/22 20:51:19 gk5g Exp $";
#endif

/*========================================================================*\
 *                                                                        *
 * implementation of  libwpi.a,                                           *
 * routines for grabbing a WP entry, and validating it                    *
 *                                                                        *
 * Sections where policy is set are indicated by "POLICY"                 *
 *                                                                        *
\*========================================================================*/

#include <andrewos.h>
#include <wpi.h>
#include <ctype.h>
#include <stdio.h>
#include <wp.h>
#include <sys/param.h>
#include <svcconf.h>
#include <util.h>
#ifdef AMS_ENV
#include <mail.h>
#endif /* AMS_ENV */
#ifdef CMU_ENV
#include <parseadd.h>
extern PARSED_ADDRESS *SingleAddress();
#endif /* CMU_ENV */


int WPI_error_code = 0;
char WPI_error_msg[2500] = "";

static char *WhichDomain = NULL; /* current working domain */

static struct wpxlate {		/* POLICY */
  char *abbrev, *full, *example;
  change_t mod;
  char *description;
} xlate_table[] = {		/* Used to translate from abbreviated 
				   fieldname to human readable format,
				   and whether or not the field may 
				   be user modified */
  { "N", "Full name", "Jello Q. Biafra", PRIVILEDGED_MODIFY,
    "User's full name, the way it should be displayed (pw_gecos)."},
  { "Tk", "Tokens", "", GENERATED_FIELD,
    "Space-separated sequence of tokens in the full name or alternate name fields."},
  { "WN", "Alternate names", "JQ Biafra;Jake Biafra", PRIVILEDGED_MODIFY,
    "Semicolon-separated sequence of alternate full names."},
  { "ID", "Login name", "jbRo", PRIVILEDGED_MODIFY,
    "The username used for logging in, mail, etc. (pw_name).  Must be unique (builds prime key)."},
  { "EK", "Source Mask", "", GENERATED_FIELD,
    "Mask giving sources.  Each bit is 1<<(pass-1), for each white-pages-build pass."},
  { "NI", "Numeric user ID", "4224", PRIVILEDGED_MODIFY,
    "Numeric ID (pw_uid)."},
  { "GI", "Group ID", "42", PRIVILEDGED_MODIFY,
    "Numeric group ID (pw_gid)."},
  { "PW", "Password", "X", PRIVILEDGED_MODIFY,
    "Encrypted password field (pw_passwd).  Use passwd(1) to change your password."},
  { "HD", "Home directory", "/usr/jbRo", PRIVILEDGED_MODIFY,
    "Home (initial login) directory (pw_dir)."},
  { "Sh", "Shell", "/bin/csh", PRIVILEDGED_MODIFY,
    "Login Shell (pw_shell)."},
  { "Af", "Affiliation", "Jello Department", PRIVILEDGED_MODIFY,
    "Affiliation (i.e. department)."},
  { "Fwd", "Mail forwarded to", "jbRo@some.do.main", ALLOW_MODIFY,
    "Mail forwarding address, if other than default mailbox."},
  { "DK", "Delivery kind", "", PRIVILEDGED_MODIFY,
    "Alternate method for local mail delivery."},
  { "DP", "Delivery parameter", "", PRIVILEDGED_MODIFY,
    "Additional information, if necessary, for the delivery kind."},
  { "D", "Surnames phonetically", "", GENERATED_FIELD,
    "Phonetically-canonicalized versions of all the surnames in the name and alternate name field."},
  { "X", "Tokens phonetically", "", GENERATED_FIELD,
    "Phonetically-canonicalized versions of all the name parts (the tokens field)."},
  { "SI", "Sequence number", "123", PRIVILEDGED_MODIFY,
    "For entries that have no User ID field, there must be a unique sequence number (builds prime key)."},
  { "NPA", "Network preferred address", "1", PRIVILEDGED_MODIFY,
    "Preference value for mail delivery."},
  { "Dt", "Last Update", "615651846", PRIVILEDGED_MODIFY,
    "Time of last update (time(0))."},
  { "A2", "Affiliation abbreviation", "jd", PRIVILEDGED_MODIFY,
    "Short form of affiliation."},
  { "HA", "Home address", "123 Main St;Anytown, PA 12345", ALLOW_MODIFY,
    "USPS address; semicolon means newline."},
  { "HP", "Home phone", "+1 412 321 4567", ALLOW_MODIFY,
    "Global telephone number"},
  { "CA", "Campus address", "UCC 100", ALLOW_MODIFY,
    "Campus mail address; semicolon means newline."},
  { "CX", "Campus extension", "x2000", ALLOW_MODIFY,
    "Campus telephone extension"},
  { "OA", "Office address", "Jello Dept.;Carnegie Mellon;Pittsburgh, PA 15213", ALLOW_MODIFY,
    "USPS address; semicolon means newline."},
  { "OP", "Office phone", "+1 412 268 2000", ALLOW_MODIFY,
    "Global telephone number."},
  { "FAX", "Telefacsimile phone (office)", "+1 412 268 5249", ALLOW_MODIFY,
    "Global telephone number."},
  { "HFX", "Telefacsimile phone (home)", "+1 412 268 5249", ALLOW_MODIFY,
    "Global telephone number."},
#ifdef CMU_ENV
  { "CAF", "Canonical Address Format", "-1", ALLOW_MODIFY,
#else /* CMU_ENV */
  { "CAF", "Canonical Address Format", "-1", PRIVILEDGED_MODIFY,
#endif /* CMU_ENV */
    "Name separator or canonical E-mail address"},
  { NULL, NULL, NULL, UNKNOWN_FIELD, NULL}
};

static void
error(msg, code)
char *msg;
int code;
{
  WPI_error_code = code;
  strcpy(WPI_error_msg,msg);
  return;
}

void
WPI_SetWorkingDomain(domain)
char *domain;
{
  WhichDomain = domain;
}

char *
WPI_GetWorkingDomain()
{
  return(WhichDomain);
}

change_t
WPI_CanIChange(field)
char *field;
{
  int i;

  for(i=0; xlate_table[i].abbrev != NULL; ++i)
    if (!strcmp(field, xlate_table[i].abbrev))
      return(xlate_table[i].mod);
  return(UNKNOWN_FIELD);
}

#ifdef AMS_ENV
static validate_t
chk_fwd(field,addr,entry)	/* POLICY -- canonicalizes forwarding addresses */
char *field;
char *addr;
WPI_entry_t entry;
{
  char *out, buf[MAXPATHLEN*2];
  int err;

  /* check the validity of the forwarding address */
  fwdvalid_SetTildeUser(WPI_Value("ID", entry));
  strcpy(fwdvalid_msgbuf, "no diagnostic");
  if ((err=ValidateFwdAddr(addr, &out))> 2) {
    sprintf(buf, "Validation error: %s", fwdvalid_msgbuf);
    error(buf, WPI_ERR_BADVALID);
    return(uncool);
  } else if (err>0) {
    sprintf(buf, "Temporary validation error: %s", fwdvalid_msgbuf);
    error(buf, WPI_TEMP_UNCERTAINVALID);
    return(uncool);
  } else {
    if (strcmp(addr,out)) {
      sprintf(buf, "Canonicalized address to `%s' from `%s'.", out, addr);
      error(buf, WPI_WARN);
    }
    WPI_Update(field, out, entry);
    return(cool);
  }
}
#endif /* AMS_ENV */

#ifdef CMU_ENV
static validate_t
chk_caf_cmuedu(field,addr,entry) /* POLICY -- allow (and canonicalize)
				    an associated CMU.EDU CMUname.
				    Also allow -1, underscore and dot */
char *field;
char *addr;
WPI_entry_t entry;
{
  char *p;
  PARSED_ADDRESS *AddrList, *Addr;
  char *out, buf[MAXPATHLEN*2];
  int err, OutMatch;
  int laType;
  char *laPrime, *laSecond, *host;
  struct wp_cd *wpCD;
  wp_PrimeKey *PKPtr;
  char *ID, *cmuWN;

  /* Strip leading and trailing whitespace */
  while (*addr && isspace(*addr)) addr++;
  p = addr + strlen(addr);
  while (p > addr && isspace(p[-1])) *p-- = '\0';

  /* Check for values that are always allowed */
  if (!strcmp(addr, "") || !strcmp(addr, "-1") || !strcmp(addr, "46")
      || !strcmp(addr, "95")) {
      WPI_Update(field, addr, entry);
      return(cool);
  }
  
  /* Check for other separator values -- they are administrator-only */
  p = addr;
  if (*p == '-') p++;
  while (*p && isdigit(*p)) p++;
  if (*p == '\0') {
      WPI_Update(field, addr, entry);
      return(drag);
  }

  /* He gave us an address, parse it */
  err = ParseAddressList(addr, &AddrList);
  if (err == PA_OK) {
      OutMatch = 0;
      Addr = SingleAddress(AddrList, &OutMatch);
      if (OutMatch != 1) {
	  FreeAddressList(AddrList);
	  err = PA_SYNTAX_ERROR;
      }
  }
  if (err == PA_NO_MEM || err == PA_PARTIAL) {
      error("Couldn't malloc memory for address parsing.",WPI_ERR_NOMEM);
      return(uncool);
  }
  else if (err == PA_SYNTAX_ERROR) {
      error("Syntax error in address.",WPI_ERR_BADVALID);
      return(uncool);
  }
  else if (err != PA_OK) {
      sprintf(buf, "Address parsing error (%d)", err);
      error(buf,WPI_ERR_BADVALID);
      return(uncool);
  }

  /* Reject bogus address types */
  err = la_Kind(Addr, &laType, &laPrime, &laSecond);
  switch (err) {
  case laerr_NoError:
      break;

  case laerr_UnrecSpecial:
      error("The text between initial plus signs in the destination address does not name a valid special address type.",WPI_ERR_BADVALID);
      FreeAddressList(AddrList);
      return(uncool);

  case laerr_SyntaxError:
      error("A local destination address is syntactically invalid.",WPI_ERR_BADVALID);
      FreeAddressList(AddrList);
      return(uncool);
  default:
      sprintf(buf, "la_Kind temp failure %d: %s.", err, la_ErrorString(err));
      error(buf, WPI_TEMP_UNCERTAINVALID);
      FreeAddressList(AddrList);
      return(uncool);
  }

  /* Addresses not at CMU.EDU are administrator-only */
  if (laType != latype_Remote || !Addr->Hosts->Prev) {
      FreeAddressList(AddrList);
      WPI_Update(field, addr, entry);
      return(drag);
  }
  if (!(host = Addr->Hosts->Prev->Name)) {
      FreeAddressList(AddrList);
      WPI_Update(field, addr, entry);
      return(drag);
  }
  lcstring(buf, host, sizeof(buf));
  if (strcmp(buf, "cmu.edu") != 0) {
      FreeAddressList(AddrList);
      WPI_Update(field, addr, entry);
      return(drag);
  }

  /*
   * Look in CMU.EDU's WP database to ensure that the address refers to the
   * user
   */
  err = wp_InitializeCell("cmu.edu", &wpCD);
  if (err != wperr_NoError) {
      FreeAddressList(AddrList);
      error(wp_ErrorString(err),WPERRPART(err));
      return(uncool);
  }

  if (p = index(Addr->LocalPart, '+')) *p = '\0';
  err = cwp_GetUIDOnly(wpCD, Addr->LocalPart, &PKPtr);
  if (err != wperr_NoError) {
      FreeAddressList(AddrList);
      error(wp_ErrorString(err),WPERRPART(err));
      cwp_Terminate(wpCD);
      return(uncool);
  }

  /* Ensure user's (andrew) userid is in the WN field of the entry */
  ID = WPI_Value("ID", entry);
  err = cwp_Read(wpCD, PKPtr, wp_FieldNameToIndex("WN"), &cmuWN);
  if (err != wperr_NoError) {
      error(wp_ErrorString(err),WPERRPART(err));
      FreeAddressList(AddrList);
      cwp_Terminate(wpCD);
      free(PKPtr);
      return(uncool);
  }

  while (cmuWN) {
      p = index(cmuWN, ';');
      if (!p) p = cmuWN + strlen(cmuWN);
      if (!strncmp(ID, cmuWN, p - cmuWN)) break;
      if (*p) {
	  cmuWN = p + 1;
      }
      else {
	  cmuWN = 0;
      }
  }
  if (!cmuWN) {
      cwp_Terminate(wpCD);
      FreeAddressList(AddrList);
      free(PKPtr);
      WPI_Update(field, addr, entry);
      return(drag);
  }
  p = Quote822Phrase(WPI_Value("N", entry));
  sprintf(buf, "%s <%s+@CMU.EDU>", p, Addr->LocalPart);
  free(p);
  FreeAddressList(AddrList);
  cwp_Terminate(wpCD);
  free(PKPtr);
  WPI_Update(field, buf, entry);
  return(cool);
}
#endif /* CMU_ENV */

static validate_t
constant_cool(field,arg,entry)
char *field;
char *arg;
WPI_entry_t entry;
{
  WPI_Update(field, arg, entry);
  return(cool);
}

static validate_t
constant_drag(field,arg,entry)
char *field;
char *arg;
WPI_entry_t entry;
{
  WPI_Update(field, arg, entry);
  return(drag);
}

static validate_t
constant_uncool(field,arg,entry)
char *field;
char *arg;
WPI_entry_t entry;
{
  return(uncool);
}

validate_t 
WPI_Validate(field, value, entry) /* POLICY */
char *field; 
char *value;
WPI_entry_t entry;
{
  static struct validate_function {
    char *field;
    validate_t (*func)();
  } vfuncs[] = {
    {"N", constant_drag},
    {"WN", constant_drag},
    {"ID", constant_drag},
    {"NI", constant_drag},
    {"GI", constant_drag},
    {"PW", constant_drag},
    {"HD", constant_drag},
    {"Sh", constant_drag},
    {"Af", constant_drag},
#ifdef AMS_ENV
    {"Fwd", chk_fwd},
#else /* AMS_ENV */
    {"Fwd", constant_cool},
#endif /* AMS_ENV */
    {"DK", constant_drag},
    {"DP", constant_drag},
    {"SI", constant_drag},
    {"NPA", constant_drag},
    {"Dt", constant_drag},
    {"A2", constant_drag},
    {"HA", constant_cool},
    {"HP", constant_cool},
    {"CA", constant_cool},
    {"CX", constant_cool},
    {"OA", constant_cool},
    {"OP", constant_cool}, 
    {"FAX", constant_cool},
    {"HFX", constant_cool},
#ifdef CMU_ENV
    {"CAF", chk_caf_cmuedu},
#else
    {"CAF", constant_drag},
#endif
   {NULL, constant_uncool}
  };
  int i;

  for(i=0;vfuncs[i].field!=NULL;++i)
    if(!strcmp(vfuncs[i].field,field))
      return(vfuncs[i].func(field,value,entry));
  return(uncool);		/* Unknown field, no changes allowed */
}

void
WPI_Update(field, value, entry)
char *field;
char *value;
WPI_entry_t entry;
{
  int i;

  for(i=0; entry[i].fieldname != NULL; ++i)
    if(!(strcmp(field, entry[i].fieldname))) {
      if((entry[i].value = (char *)malloc(strlen(value)+1))==NULL) {
	error("Couldn't malloc memory for updated value.",WPI_ERR_NOMEM);
	return;
      }
      strcpy(entry[i].value,value);
      entry[i].changed = true;
      return;
    }
}

char *
WPI_Value(field, entry)
char *field;
WPI_entry_t entry;
{
  int i;

  for(i=0; entry[i].fieldname != NULL; ++i)
    if(!(strcmp(field, entry[i].fieldname)))
      return(entry[i].value);
  return(NULL);
}

char *
WPI_Nice(fieldname)
char *fieldname;
{
  int i;

  for(i=0; xlate_table[i].abbrev != NULL; ++i)
    if (!strcmp(fieldname, xlate_table[i].abbrev))
      return(xlate_table[i].full);
  return(fieldname);
}

char *
WPI_Example(fieldname)
char *fieldname;
{
  int i;

  for(i=0; xlate_table[i].abbrev != NULL; ++i)
    if (!strcmp(fieldname, xlate_table[i].abbrev))
      return(xlate_table[i].example);
  return("");
}

char *
WPI_Description(fieldname)
char *fieldname;
{
  int i;

  for(i=0; xlate_table[i].abbrev != NULL; ++i)
    if (!strcmp(fieldname, xlate_table[i].abbrev))
      return(xlate_table[i].description);
  return("");
}

char *
WPI_Self()
{
  struct CellAuth *ca;
  int err;

  if (WhichDomain == NULL) {
    CheckServiceConfiguration();
    WhichDomain = ThisDomain;
  }
  if ((err=FindCell(WhichDomain, &ca))!=0) {
    if (err==1) return(NULL);
    else if (err=2) {
      error("Not authenticated.", WPI_ERR_NOTAUTH);
      return(NULL);
    } else {
      error("Couldn't FindCell(WhichDomain).",WPI_ERR_NOCELL);
      return(NULL);
    }
  }
  FillInCell(ca);
  return(ca->UserName);
}
#define WPCALL(x) {wp_ErrorCode err; if ((err=(x))!=wperr_NoError) {error(wp_ErrorString(err),WPERRPART(err));if (wpd) cwp_Terminate(wpd); wpd=NULL; return(NULL);}}

WPI_entry_t
WPI_Lookup(user, admin_flag)
char *user;
boolx_t admin_flag;
{
  static struct wp_cd *wpd = NULL; /* "sticky" fields--try to maintain  */
  static char *wpd_domain = NULL; /* state from call to call */
  static int num_fields = 0;

  wp_PrimeKey primekey;
  wp_ErrorCode err;
  WPI_entry_t entry;
  char *temp, *fldvalue;
  int i, j, k;
  change_t access;

  if (!WhichDomain) {		/* Default to ThisDomain, if not specified */
    error("Setting default domain to ThisDomain.", WPI_WARN);
    CheckServiceConfiguration();
    WhichDomain = ThisDomain;
  }

  if (!wpd || (strcmp(WhichDomain, wpd_domain))) { 
    /* need to initialize WP */
    wpd_domain = WhichDomain;
				/* Set up the WP */
    WPCALL(wp_InitializeCell(WhichDomain, &wpd));

				/* Compute the number of fields */
    for(num_fields=0,err=wperr_NoError; err==wperr_NoError; ++num_fields)
      err=wp_AllFields(num_fields, &temp);

    if (err!=wperr_FieldIndexOutOfBounds) {
      error(wp_ErrorString(err),WPERRPART(err));
      cwp_Terminate(wpd);
      wpd = NULL;
      return(NULL);
    }
    --num_fields;
  }
				/* find a key */
  primekey = NULL;
  WPCALL(cwp_GetUIDOnly(wpd,user, &primekey));
  if(primekey==NULL) {
    error("Couldn't get key.",WPI_ERR_NOKEY);
    cwp_Terminate(wpd);
    wpd = NULL;
    return(NULL);
  }

				/* malloc some memory for the entries */
  if ((entry = (WPI_entry_t)malloc((num_fields+1)*sizeof(*entry)))==NULL) {
    error("Couldn't malloc space for an entry.",WPI_ERR_NOMEM);
    cwp_Terminate(wpd);
    wpd = NULL;
    return(NULL);
  }

  for(j=i=0; i < num_fields; ++i) {
    WPCALL(wp_AllFields(i, &temp));

	if ((entry[j].fieldname=(char *)malloc(strlen(temp)+1))==NULL) {
	  error("Couldn't malloc space for fieldname.",WPI_ERR_NOMEM);
	  cwp_Terminate(wpd);
	  wpd = NULL;
	  for(k=0;k<j;++k) if(entry[k].fieldname) free(entry[k].fieldname);
	  free(entry);
	  return(NULL);
	}
	strcpy(entry[j].fieldname,temp);
	entry[j].changed = false;
	entry[j++].fieldnum = i;
  }
  entry[j].fieldnum = -1; /* mark the end of the entry */
  entry[j].fieldname = NULL;

				/* read the relevant fields */
  for(k=0; entry[k].fieldnum != -1; ++k) {
    switch (err=cwp_Read(wpd,primekey,entry[k].fieldnum,&fldvalue)) {
    case wperr_NoError:
      if((entry[k].value = (char *)malloc(strlen(fldvalue)+1))==NULL) {
	error("Couldn't malloc memory for value.",WPI_ERR_NOMEM);
	cwp_Terminate(wpd);
	wpd = NULL;
	for(i=0;i<num_fields;++i) if(entry[i].fieldname) free(entry[i].fieldname);
	for(i=0;i<k;++i) if(entry[i].value) free(entry[i].value);
	free(entry);
	return(NULL);
      }
      strcpy(entry[k].value, fldvalue);
      break;
    case wperr_NoSuchField:
      entry[k].value = NULL;
      break;
    default:
      error(wp_ErrorString(err),WPERRPART(err));
      cwp_Terminate(wpd);
      wpd = NULL;
      for(i=0;i<num_fields;++i) if(entry[i].fieldname) free(entry[i].fieldname);
      for(i=0;i<k;++i) if(entry[i].value) free(entry[i].value);
      free(entry);
      return(NULL);
    }
  }
	   
  return(entry);
}

#undef WPCALL
