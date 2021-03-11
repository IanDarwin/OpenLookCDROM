/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
class month:dataobject[dataobj] {
    overrides:
      Read (FILE *file, long id) returns long;
      Write (FILE *file, long writeid, int level) returns long;
    methods:
      SetMonthAndYear(int mon, int year);
    macromethods:
      GetMonth() (self->mon)
      GetYear() (self->year)
    data:
      int mon, year;
};
