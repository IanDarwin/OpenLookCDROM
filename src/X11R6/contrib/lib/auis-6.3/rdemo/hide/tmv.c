#include <tmv.eh>

boolean tmview__InitializeClass(c)
struct classheader *c;
{
    return (TRUE);
}

boolean tmview__InitializeObject(c, self)
struct classheader *c;
struct tmview *self;
{
    return (TRUE);
}

void tmview__FinalizeObject(c, self)
struct classheader *c;
struct tmview *self;
{
}
