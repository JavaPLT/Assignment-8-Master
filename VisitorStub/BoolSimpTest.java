 import junit.framework.*;

public class BoolSimpTest extends TestCase {
  
  public final static Variable x = Variable.makeVariable("X");
  public final static Variable y = Variable.makeVariable("Y");
  public final static Variable z = Variable.makeVariable("Z");
  
  public final static Form f1 = new And(x,y);
  public final static IfForm if1 = new IfIf(x, y, Constant.FALSE);
  
  public final static Form f2 = new Or(x,y);
  public final static IfForm if2 = new IfIf(x, Constant.TRUE, y);
  
  public final static Form f3 = new Implies(x,y);
  public final static IfForm if3 = new IfIf(x, y, Constant.TRUE);
  
  public final static Form f4 = new Not(z);
  public final static IfForm if4 = new IfIf(z, Constant.FALSE, Constant.TRUE);
  
  public final static Form f5 = new If(x, y, z);
  public final static IfForm if5 = new IfIf(x, y, z);
  
  public final static Form f6 = new Or(new And(x,y), new Or(y,z));
  public final static IfForm if6 = new IfIf(new IfIf(x, y, Constant.FALSE), 
                               Constant.TRUE, 
                               new IfIf(y, Constant.TRUE, z));
  
  public final static Form f7 = new And(new Or(x,y), new And(y,z));
  public final static IfForm if7 = new IfIf(new IfIf(x, Constant.TRUE, y),  
                               new IfIf(y, z, Constant.FALSE),
                               Constant.FALSE);
  
  public final static Form f8 = new Implies(new Implies(x,y), new And(y,z));
  public final static IfForm if8 = new IfIf(new IfIf(x, y, Constant.TRUE),
                               new IfIf(y, z, Constant.FALSE),
                               Constant.TRUE);
  
  public final static IfForm nif8 = new IfIf(x, 
                                new IfIf(y, 
                                         new IfIf(y, z, Constant.FALSE), 
                                         Constant.TRUE),
                                new IfIf(Constant.TRUE, 
                                         new IfIf(y, z, Constant.FALSE), 
                                         Constant.TRUE));
  public final static IfForm enif8 = new IfIf(x, 
                                 new IfIf(y, 
                                          z, 
                                          Constant.TRUE),
                                 new IfIf(y, z, Constant.FALSE));
  
  public final static IfForm if9 = new IfIf(if8, x, y);
  public final static IfForm nif9 = new IfIf(x,                         
                                new IfIf(y, 
                                         new IfIf(y, 
                                                  new IfIf(z, x, y), 
                                                  new IfIf(Constant.FALSE, x, y)),
                                         new IfIf(Constant.TRUE, x, y)),                         
                                new IfIf(Constant.TRUE, 
                                         new IfIf(y, 
                                                  new IfIf(z, x, y), 
                                                  new IfIf(Constant.FALSE, x, y)),
                                         new IfIf(Constant.TRUE, x, y)));
  
  public final static IfForm enif9 = new IfIf(x,                          
                                 Constant.TRUE,
                                 new IfIf(y, 
                                          new IfIf(z, Constant.FALSE, Constant.TRUE), 
                                          Constant.FALSE));
  
  public final static IfForm nif10 = new IfIf(x, y, y);
  public final static IfForm enif10 = y;
  
  public final static IfForm nif11 = new IfIf(x, Constant.TRUE, Constant.FALSE);
  public final static IfForm enif11 = x;
  
  public BoolSimpTest(final String name) { super(name); }
  
  public void setup() { }
  
  public void testEquals() {
    assertEquals("Variable equals", x, Variable.makeVariable("X"));
    assertTrue("Variable x not equals Variable y", !(x.equals(y)));
    assertEquals("And equals", f1, new And(x,y));
    assertEquals("Or equals", f2, new Or(x,y));
    assertEquals("Implies equals", f3, new Implies(x,y));
    assertEquals("Not equals", f4, new Not(z));
    assertEquals("If equals", f5, new If(x, y, z));
  }
  
  public void testConvertToIf() {
    // ConvertToIf.ONLY is a static field in ConvertToIf that is bound to new ConvertToIf()
    assertEquals("Variable Conversion", x, x.accept(ConvertToIf.ONLY));
    assertEquals("And Conversion", if1, f1.accept(ConvertToIf.ONLY));
    assertEquals("Or Conversion", if2, f2.accept(ConvertToIf.ONLY));
    assertEquals("Implies Conversion", if3, f3.accept(ConvertToIf.ONLY));
    assertEquals("Not Conversion", if4, f4.accept(ConvertToIf.ONLY));
    assertEquals("If Conversion", if5, f5.accept(ConvertToIf.ONLY));
    assertEquals("Compound Or Conversion", if6, f6.accept(ConvertToIf.ONLY));
    assertEquals("Compound And Conversion", if7, f7.accept(ConvertToIf.ONLY));
    assertEquals("Compound Implies Conversion", if8, f8.accept(ConvertToIf.ONLY)); 
  }
  
  public void testNorm() {
    // Normalize.ONLY is a static field in Normalize that is bound to new Normalize()
    assertEquals("Normalize Variable", x, x.accept(Normalize.ONLY));
    assertEquals("Normalize Constant.TRUE", Constant.TRUE, Constant.TRUE.accept(Normalize.ONLY));
    assertEquals("Normalize Constant.FALSE", Constant.FALSE, Constant.FALSE.accept(Normalize.ONLY));
    assertEquals("Normalize Trivial If", if5, if5.accept(Normalize.ONLY));
    assertEquals("One-step Normalize", nif8, if8.accept(Normalize.ONLY));
    assertEquals("Nested Normalize", nif9, if9.accept(Normalize.ONLY));
  }
  
  public void testEval() {
    // Evaluate.BASE is a static field in Evaluate that is bound to new Evaluate(EmptyEnvironment.ONLY)
    assertEquals("Eval Variable", x, x.accept(Evaluate.BASE));
    assertEquals("Eval Trivial If", if5, if5.accept(Evaluate.BASE));
    assertEquals("Eval nif8", enif8, nif8.accept(Evaluate.BASE));
    assertEquals("Eval nif9", enif9, nif9.accept(Evaluate.BASE));
    assertEquals("Check (? x alpha alpha) => alpha", enif10, nif10.accept(Evaluate.BASE));
    assertEquals("Check (? x T F) => x", enif11, nif11.accept(Evaluate.BASE));
  }
  
  public void testConvertToBool() {
    // ConvertToBool.ONLY is a static field in ConvertToBool that is bound to new ConvertToBool()
    assertEquals("ConvertToBool variable", x.accept(ConvertToBool.ONLY), x);
  }

  /** This method is automatically generated by the Language Level Converter. */
  public BoolSimpTest() {
    super();
  }

  /** This method is automatically generated by the Language Level Converter. */
  public java.lang.String toString() {
    return getClass().getName() + "(" + 
        ")";
  }

  /** This method is automatically generated by the Language Level Converter. */
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    else if ((o == null) || (! o.getClass().equals(getClass()))) {
      return false;
    }
    else {
      BoolSimpTest cast = ((BoolSimpTest) o);
      return true;
    }
  }

  /** This method is automatically generated by the Language Level Converter. */
  public int hashCode() {
    return getClass().hashCode();
  }
}  
