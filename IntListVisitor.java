/** Visitor interface for the IntList type. */
interface IntListVisitor {
  abstract Object forEmptyIntList(EmptyIntList host);
  abstract Object forConsIntList(ConsIntList host);
}