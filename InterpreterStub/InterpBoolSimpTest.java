import junit.framework.*;

public class InterpBoolSimpTest extends TestCase {
  
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
  
  public InterpBoolSimpTest(final String name) { super(name); }
  
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
    assertEquals("Variable Conversion", x, x.convertToIf());
    assertEquals("And Conversion", if1, f1.convertToIf());
    assertEquals("Or Conversion", if2, f2.convertToIf());
    assertEquals("Implies Conversion", if3, f3.convertToIf());
    assertEquals("Not Conversion", if4, f4.convertToIf());
    assertEquals("If Conversion", if5, f5.convertToIf());
    assertEquals("Compound Or Conversion", if6, f6.convertToIf());
    assertEquals("Compound And Conversion", if7, f7.convertToIf());
    assertEquals("Compound Implies Conversion", if8, f8.convertToIf()); 
  }
  
  public void testNorm() {
    // Normalize.ONLY is a static field in Normalize that is bound to new Normalize()
    assertEquals("Normalize Variable", x, x.normalize());
    assertEquals("Normalize Constant.TRUE", Constant.TRUE, Constant.TRUE.normalize());
    assertEquals("Normalize Constant.FALSE", Constant.FALSE, Constant.FALSE.normalize());
    assertEquals("Normalize Trivial If", if5, if5.normalize());
    assertEquals("One-step Normalize", nif8, if8.normalize());
    assertEquals("Nested Normalize", nif9, if9.normalize());
  }
  
  public void testEval() {
    Environment E = EmptyEnvironment.ONLY;
    assertEquals("Eval Variable", x, x.eval(E));
    assertEquals("Eval Trivial If", if5, if5.eval(E));
    assertEquals("Eval nif8", enif8, nif8.eval(E));
    assertEquals("Eval nif9", enif9, nif9.eval(E));
    assertEquals("Check (? x alpha alpha) => alpha", enif10, nif10.eval(E));
    assertEquals("Check (? x T F) => x", enif11, nif11.eval(E));
  }
  
  public void testConvertToBool() {
    // ConvertToBool.ONLY is a static field in ConvertToBool that is bound to new ConvertToBool()
    assertEquals("ConvertToBool variable", x.convertToBool(), x);
  }
  
  
  public void testPrint() {
    // Print.ONLY is a static field in Print that is bound to new Print()
    Form boolForm = new And(new Implies(x, y), new Or(z, new If(z, y, x)));
    assertEquals("ComplexBoolFormula", "(& (> X Y) (| Z (? Z Y X)))", boolForm.print());
    IfForm ifForm = new IfIf(new IfIf(x, y, Constant.TRUE), new IfIf(z, Constant.TRUE, new IfIf(z, y, x)), Constant.FALSE);
    assertEquals("ComplexIfFormula", "(? (? X Y T) (? Z T (? Z Y X)) F)", ifForm.print());
  }
}  
