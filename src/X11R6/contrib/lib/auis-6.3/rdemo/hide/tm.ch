class termulator[tm]: text {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct termulator *self) returns boolean;
    FinalizeObject(struct termulator *self);
  overrides:
    ViewName() returns char *;
};
