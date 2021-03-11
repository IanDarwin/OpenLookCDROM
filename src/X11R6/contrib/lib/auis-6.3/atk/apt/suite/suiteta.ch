/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
class suitetapp [suiteta]: application [app]
  {
  overrides:
   Start()				    returns boolean;
   ParseArgs( long argc, char **argv )	    returns boolean;
  };
