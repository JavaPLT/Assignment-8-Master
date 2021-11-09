// Java Version: 8.0
import java.util.*;

/* The definitions of the basic formula interfaces, Form and IfForm, appear below. Interfaces are used rather 
 * than abstract classes so that the Constant and Variable classes can be subtypes in BOTH hierarchies. The equals
 * method is overrideen in Form and IfForm types where necessary to achieve structural equality. */

/** Form ::= Constant | Variable | Not(Form) | And(Form, Form) | Or(Form, Form) | Implies(Form, Form) | 
  *          If(Form, Form, Form)                                                                            
  * This syntax (in abbreviated form described in class Parser) is used for the input and output of boolean formulas. */

/* interface definition for form */
interface Form {
  /* The visitor pattern hook for the Form type. */
  public <R> R accept(FormVisitor<R> v);  // Form visitor pattern hook
}

/* Class representing variables in Forn and IfForm composite hierarchies.  The Variable object for a particular 
 * String x is unique due to maintaining a hash table. */
class Variable implements Form, IfForm { 
  private String name;
  
  static final HashMap<String,Variable> symbolTable = new HashMap<String,Variable>();
  
  public static Variable makeVariable(String name) {
    
    Variable result = symbolTable.get(name);
    if (result == null) {
      result = new Variable(name);
      symbolTable.put(name,result);
    }
    return result;
  }
  
  private Variable(String name) { this.name = name; }  // "replaced" by unique object creator makeVariable

  public String name() { return name; }
  public String toString() { return name; }
  
  /** The default meaning of equals corresponds to structual equality on the leaf values in Form */
  // public boolean equals(Object o);
  // default equals on Object compares pointers which is correct
  
  public <R> R accept(FormVisitor<R> v) { return v.forVariable(this); }
  public <R> R accept(IfFormVisitor<R> v) { return v.forVariable(this); }
}

class Constant implements Form,IfForm {
  
  public static final Constant TRUE = new Constant(true);     // generalized Singleton pattern
  public static final Constant T = TRUE;
  public static final Constant FALSE = new Constant(false);
  public static final Constant F = FALSE;
  
  private boolean value;
  
  private Constant(boolean value) { this.value = value; }
  
  public boolean value() { return value; }
  
  public String toString() { return value ? "T" : "F"; }
  
    public <R> R accept(FormVisitor<R> v) { return v.forConstant(this); }
    public <R> R accept(IfFormVisitor<R> v) { return v.forConstant(this); }
  
  // public boolean equals(Object o);
  // default equals on Object compares pointers which is correct
}

class Not implements Form {
  private Form arg;
  
  public Not(Form a) { arg = a; }
  Form arg() { return arg; }
  
  public <R> R accept(FormVisitor<R> v) { return v.forNot(this); }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Not) {
      Not no = (Not) o;
      return arg.equals(no.arg());
    }
    else return false; 
  }
}

class And implements Form {
  private Form left,right;
  public And(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() {return right; }
  public <R> R accept(FormVisitor<R> v) { return v.forAnd(this); }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof And) {
      And io = (And) o;
      return left.equals(io.left()) && right.equals(io.right());
    }
    else return false; 
  }
}

class Or implements Form {
  private Form left,right; 
  public Or(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() { return right; }
  public <R> R accept(FormVisitor<R> v) { return v.forOr(this); }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Or) {
      Or oo = (Or) o;
      return left.equals(oo.left()) && right.equals(oo.right());
    }
    else return false; 
  }
}

class Implies implements Form {
  private Form left,right;
  public Implies(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() { return right; } 
  public <R> R accept(FormVisitor<R> v) { return v.forImplies(this); }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Implies) {
      Implies io = (Implies) o;
      return left.equals(io.left()) && right.equals(io.right());
    }
    else return false; 
  }
}

class If implements Form {
  
  private Form test,conseq,alt;
  
  public If(Form test, Form conseq, Form alt) {
    this.test = test;
    this.conseq = conseq;
    this.alt = alt;
  }
  public Form test() { return test; }
  public Form conseq() { return conseq; }
  public Form alt() { return alt; }
    
  public <R> R accept(FormVisitor<R> v) { return v.forIf(this); }
  
  public boolean equals(Object o) {
    if (o instanceof If) {
      If io = (If) o;
      return test.equals(io.test()) && conseq.equals(io.conseq()) && alt.equals(io.alt());
    }
    else return false; 
  }
}

// Potential Optimization: only use IfForm for normalized formulas, implying test is a variable or constant.
/** IfForm ::= Constant | Variable | IfIf(IfForm, IfForm, IfForm) */

/* interface definition of IfForm */
interface IfForm {
  /* The visitor pattern hook for the IfForm type. */
  public <R> R accept(IfFormVisitor<R> v);
}

/* Variable and Constant already defined above */

/* This class is essentially a copy of class If but with more accurate typing to simplify IfForm computations. */
class IfIf implements IfForm {
  
  private IfForm test,conseq,alt;
  
  public IfIf(IfForm test, IfForm conseq, IfForm alt) {
    this.test = test;
    this.conseq = conseq;
    this.alt = alt;
  }
  
  public IfForm test() { return test; }
  public IfForm conseq() { return conseq; }
  public IfForm alt() { return alt; }
  
  /** By overridding equals here, we complete the definition of structural equality on type IfForm */
  public boolean equals(Object o) {
    if (o instanceof IfIf) {
      IfIf io = (IfIf) o;
      return test.equals(io.test()) && conseq.equals(io.conseq()) && alt.equals(io.alt());
    }
    else return false; 
  }
  
  public <R> R accept(IfFormVisitor<R> v) { return v.forIfIf(this); }
}

interface FormVisitor<R> {

  R forConstant(Constant host);
  R forVariable(Variable host);
  R forNot(Not host);
  R forAnd(And host);
  R forOr(Or host);
  R forImplies(Implies host);
  R forIf(If host);
}

interface IfFormVisitor<R> {
  
  R forConstant(Constant host);
  R forVariable(Variable host);
  R forIfIf(IfIf host);  // an If with IfForm subForms 
}

/* An Environment is a list of bindings of variables to constants (instances of Constant). */
abstract class Environment {

  Environment bind(Variable v, Constant c) { return new AddBinding(v,c,this); }
  
  /** Returns the matching Constant bound to v.  If no such Constant is found, returns null. */
  abstract public Constant lookup(Variable v);
}

/** The Empty Environment class, akin to Scheme empty */
class EmptyEnvironment extends Environment {

  static final EmptyEnvironment ONLY = new EmptyEnvironment();   // Singleton pattern
  private EmptyEnvironment() { }
  
  public Constant lookup(Variable v) { return null; }
}
  
/** The non-empty Environment class, akin to Scheme cons. */
class AddBinding extends Environment {
  private Variable sym;
  private Constant value;
  private Environment rest;
  
  public AddBinding(Variable v, Constant c, Environment e) {
    sym = v;
    value = c;
    rest = e;
  }
  
  public Variable sym() { return sym; }
  public Constant value() { return value; }
  public Environment rest() { return rest; }
  
  public Constant lookup(Variable s) {
    if (s.equals(sym)) return value;
    else return rest.lookup(s);
  }
}

///** Visitor for convertToIf method. */
//class ConvertToIf implements FormVisitor<IfForm> {
//  
//  static final ConvertToIf ONLY = new ConvertToIf();  // Singleton pattern
//  private ConvertToIf() { }
//  
//  public IfForm forVariable(Variable s) { return s; }
//  public IfForm forConstant(Constant c) { return c; } 
//  
//  ...
//}

///** class Visitor for the headNormalize method. */
//class Normalize implements IfFormVisitor<IfForm> {
//  
//  static final Normalize ONLY = new Normalize();   // Singleton pattern
//  private Normalize() { }
// ...
//}

///* Visitor class for the headNormalize method. */
//class HeadNormalize implements IfFormVisitor<IfForm> {
//  
//  // test, conseq, alt are already normalized b
//  IfForm test, conseq, alt;
//  
//  public HeadNormalize(IfForm test, IfForm conseq, IfForm alt) {
//    this.test = test;
//    this.conseq = conseq;
//    this.alt = alt;
//  }     
//  ...
//}

/** Visitor class for evaluate method */
class Evaluate implements IfFormVisitor<IfForm> {
  
  static final Environment empty = EmptyEnvironment.ONLY;
  Environment env;
  
  static final Evaluate BASE = new Evaluate(empty);  // akin to Singleton, but recursive calls cannot use BASE
  private Evaluate(Environment e) { env = e; } 
  /* ... */
}  

/** Visitor for the convertToBool method */
class ConvertToBool implements IfFormVisitor<Form> {
   
  static final ConvertToBool ONLY = new ConvertToBool();  // Singleton patter
  private ConvertToBool() { }
  /* ... */
}
  
class Print implements FormVisitor<String>, IfFormVisitor<String> {
  
  String print(Form f) { return (String) f.accept(this); }
  String print(IfForm f) { return (String) f.accept(this); }
  
  static final Print ONLY = new Print();
  
  public String forVariable(Variable v) { return v.name(); }
  
  public String forConstant(Constant c) {
    if (c.value()) return "T";
    else return "F"; 
  }
  
  public String forIf(If i) {
    return  "(? " + print(i.test()) + " " + print(i.conseq()) + " " + print(i.alt()) + ")";
  }
  
  public String forIfIf(IfIf i) {
    return  "(? " + print(i.test()) + " " + print(i.conseq()) + " " + print(i.alt()) + ")";
  }
  
  public String forNot(Not n) { return "(! " + print(n.arg()) + ")"; }
  public String forAnd(And a) { return "(& " + print(a.left()) + " " + print(a.right()) + ")"; }       
  public String forOr(Or o) { return "(| " + print(o.left()) + " " + print(o.right()) + ")"; }
  public String forImplies(Implies i) { return "(> " + print(i.left()) + " " + print(i.right()) + ")"; }
}