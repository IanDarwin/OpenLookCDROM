class rdemosh : application [app] {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct rdemosh *self) returns boolean;
    FinalizeObject(struct rdemosh *self);
  overrides:
    Start() returns boolean;
    Run() returns int;
    ParseArgs(int argc, char **argv) returns boolean;
  data:
    char email[100];
    boolean timeup;
    int numbuttons;
    struct frame *Frame;
    struct text *Text;
    struct pushbutton **buttons;
    struct text **buttontext;
};
