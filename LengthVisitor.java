/** IntListVisitor class that implements the length() method on IntLists. */
 class LengthVisitor implements IntListVisitor {
  /* Singleton binding for LengthVisior because the method takes no parameters */
  public static final LengthVisitor ONLY = new LengthVisitor();  
  private LengthVisitor() { }
  
  public Object forEmptyIntList(EmptyIntList el) { return 0; }
  public Object forConsIntList(ConsIntList cil) { return 1 + (Integer) cil.rest().visit(this); }
}