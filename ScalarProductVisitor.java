/** IntListVistor that implements the scalarProduct "method" on IntLists. */
class ScalarProductVisitor implements IntListVisitor {
  /** other is field storing the "argument" to the scalarProduct "method"; assumed equal in length to the "host". */
  private IntList other;
  ScalarProductVisitor(IntList o) { other = o; }
  
  public Object forEmptyIntList(EmptyIntList el) { return 0; }
  public Object forConsIntList(ConsIntList cil) {
    ConsIntList otherCons = (ConsIntList) other;  // cast other to a ConsIntList
    return (cil.first)*(otherCons.first()) + (Integer) cil.rest().visit(new ScalarProductVisitor(otherCons.rest())); 
  }
}