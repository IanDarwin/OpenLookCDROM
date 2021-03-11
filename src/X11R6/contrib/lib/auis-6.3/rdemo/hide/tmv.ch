class tmview[tmv]: textview[textv] {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct tmview *self) returns boolean;
    FinalizeObject(struct tmview *self);
};
