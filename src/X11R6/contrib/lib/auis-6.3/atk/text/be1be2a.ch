/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
 * BE1 to BE2 conversion utility
 */

class be1be2app[be1be2a]: application[app] {
overrides:
    ParseArgs(int argc, char **argv) returns boolean;
    Run() returns boolean;
};
