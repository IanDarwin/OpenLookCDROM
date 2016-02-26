/* C++ Definitions file for genix objects. */

class Person {
public:
	Person(char *gname, char *sname,
		char *bdate, char *bplace, char *ddate, char *dplace);
	Person(char *gname,char *sname);
	~Person();
	char *GetName();	// returns pname
	Person& operator=(const Person&);
private:
	char *gname;
	char *sname;
	char pname[PRTNAMELEN];		/* printable */
	char *bdate, *bplace;
	char *ddate, *dplace;
	/* etc. */
	// Couple *parents;		/* up-link */
	// Couple *couple;		/* self-wedding */
	Person *sibling;	/* next-link in child list */
	Person *next;		/* next-link in main list */
	void setName(char *gname, char *sname);
	void buildName();	/* make prtname from gname, sname */
};

/* A Couple is any cohabital union that results in offspring;
 * no religious or moral overtones meant or intended.
 * "Just the facts, ma'am."
 */
class Couple {
public:
	Couple(Person *h, Person *w);
	~Couple();
	void AddKid(Person *k);
private:
	Person *husband, *wife;
	Person *child;
};
