#include <tscript.eh>

boolean typescript__InitializeClass(c)
struct classheader *c;
{
    return (TRUE);
}

boolean typescript__InitializeObject(c, self)
struct classheader *c;
struct typescript *self;
{
    return (TRUE);
}

void typescript__FinalizeObject(c, self)
struct classheader *c;
struct typescript *self;
{
}
