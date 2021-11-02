import junit.framework.TestCase;  // imported from the class files in junit.jar
/** A JUnit test case class.
  * Every method starting with the word "test" will be called when running this test class with JUnit.
  */
public class IntListTest extends TestCase {
  
  EmptyIntList e = IntList.empty();
  IntList l1 = e.cons(7).cons(5).cons(3);
  IntList l2 = e.cons(3).cons(7).cons(5);  // structurally equal to l5 below
  IntList l3 = IntList.countDown(50);
  IntList l4 = IntList.countUp(50);
  IntList l5 = e.cons(3).cons(7).cons(5);  // structurally equal to l2 above

  
  /** Tests Lisp-like toString() method */
  public void testToString() {
    assertEquals("empty list string", "()", e.toString());
    assertEquals("Non-empty list string", l1.toString(), l2.sort().toString());
  }
  
  /** Tests equals(Object o) method */
  public void testEquals() {
    assertTrue("empty list reflexivity", e.equals(e));
    assertTrue("Non-empty IntList", l2.equals(l5));
  }
  
  /** Tests sort() method */
  public void testSort() {
    assertEquals("empty sort", e, e.sort());
    assertEquals("sort inductive test", l1.toString(), l2.sort().toString());
    assertEquals("big sort test [toString]", l4.toString(), l3.sort().toString());  // toString() equality
    assertEquals("big sort test", l4, l3.sort());
  }
                 
  /** Tests LengthVisitor.  */
  public void testLength() {
    assertEquals("empty list test", e, EmptyIntList.ONLY );
    assertEquals("LengthVisitor empty test", 0, e.visit(LengthVisitor.ONLY));
    assertEquals("LengthVisitor inductive test", 3, l1.visit(LengthVisitor.ONLY));
  }
  
  /** Tests ScalarProductVisitor. */
  public void testScalaProduct() {
    assertEquals("ScalarProductVisitor empty test", 0, e.visit(new ScalarProductVisitor(e)));
    assertEquals("LengthVisitor inductive test", 71, l1.visit(new ScalarProductVisitor(l2)));
  }
}
