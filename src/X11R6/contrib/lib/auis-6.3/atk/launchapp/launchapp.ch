class launchapp : application [app] {
  classprocedures:
    InitializeObject(struct launchapp *self) returns boolean;
    FinalizeObject(struct launchapp *self);
  overrides:
    Start() returns boolean;
    Run() returns int;
  data:
    char *TourFile;
    struct pushbutton **buttons;
    struct text **buttontext;
    struct frame *frame;
    int numbuttons;
};
