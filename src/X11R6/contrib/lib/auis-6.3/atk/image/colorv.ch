class colorv : view {
classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct colorv *self) returns boolean;
  FinalizeObject(struct colorv *self);
overrides:
  FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
  Update();
};

