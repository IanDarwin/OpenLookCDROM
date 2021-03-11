class typetext: text {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct typetext *self) returns boolean;
    FinalizeObject(struct typetext *self);
  overrides:
    ViewName() returns char *;
};
