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
  /** @return the IfForm equivalent to this. */
  IfForm convertToIf();
  /** @return String representation of this. */
  String print();
}

/* interface definition of IfForm */
interface IfForm {
  
  /** @return the normalized IfForm equivalent to this.  A normalized IfForm only contains atoms (Constant | Variable) 
    * in test position. */
  IfForm normalize();
  
  /** @return the normalized form equivaent to new If(this, conseq, alt), assuming the formulas this, conseq, and alt
    * are already normalized. */
  IfForm headNormalize(IfForm conseq, IfForm alt);
  
  /** @return the "simplest" normalized form for this, assuming this is already normalized.  Tautologies are reduced to 
    * Constant.TRUE; contradictions are reduced to Constant.FALSE.  Other formulas are reduced according to an ordered 
    * set of reduction rules. */
  IfForm eval(Environment env);
  
  /** @return a Form f containing the fewest possible If constructions such that f.convertToIf() = this. */
  Form convertToBool();
  
  /** @return String representation of this. */
  String print();
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
      symbolTable.put(name, result);
    }
    return result;
  }
  
  private Variable(String name) { this.name = name; }  // "replaced" by unique object creator makeVariable

  public String name() { return name; }
  public String toString() { return name; }
  
  /** The default meaning of equals corresponds to structual equality on the leaf values in Form */
  // public boolean equals(Object o);
  // default equals on Object compares pointers which is correct
  
  public IfForm convertToIf() { ... }
  public IfForm normalize() { ... }
  public IfForm headNormalize(IfForm conseq, IfForm alt) { ... }
  public IfForm eval(Environment env) { ... }
  public Form convertToBool() { ... }
  
  public String print() { return name; }
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
  
  // public boolean equals(Object o);
  // default equals on Object compares pointers which is correct
  
  public IfForm convertToIf() { ... }
  public IfForm normalize() { ... }
  public IfForm headNormalize(IfForm conseq, IfForm alt) { ... }
  public IfForm eval(Environment env) { ... }
  public Form convertToBool() { ... }
  
  public String print() { return value ? "T" : "F"; }
}

class Not implements Form {
  private Form arg;
  
  public Not(Form a) { arg = a; }
  Form arg() { return arg; }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Not) {
      Not no = (Not) o;
      return arg.equals(no.arg());
    }
    else return false; 
  }
  public IfForm convertToIf() { ... }

  public String print() { return "(! " + arg.print() + ")"; }
}

class And implements Form {
  private Form left,right;
  public And(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() {return right; }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof And) {
      And io = (And) o;
      return left.equals(io.left()) && right.equals(io.right());
    }
    else return false; 
  }
  
  public IfForm convertToIf() { ... }
  
  public String print() { return "(& " + left.print() + " " + right.print() + ")"; } 
}

class Or implements Form {
  private Form left,right; 
  public Or(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() { return right; }
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Or) {
      Or oo = (Or) o;
      return left.equals(oo.left()) && right.equals(oo.right());
    }
    else return false; 
  }
    
  public IfForm convertToIf() { ... }
  
  public String print() { return "(| " + left.print() + " " + right.print() + ")";  }
}

class Implies implements Form {
  private Form left,right;
  public Implies(Form l, Form r) {
    left = l;
    right = r;
  }
  public Form left() { return left; }
  public Form right() { return right; } 
  
  /** Overridding equals here to support the definition of structural equality on type Form */
  public boolean equals(Object o) {
    if (o instanceof Implies) {
      Implies io = (Implies) o;
      return left.equals(io.left()) && right.equals(io.right());
    }
    else return false; 
  }
  
  public IfForm convertToIf() { ... }
  
  public String print() { return "(> " + left.print() + " " + right.print() + ")"; }
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
  
  public boolean equals(Object o) {
    if (o instanceof If) {
      If io = (If) o;
      return test.equals(io.test()) && conseq.equals(io.conseq()) && alt.equals(io.alt());
    }
    else return false; 
  }
  
  public IfForm convertToIf() { ... }
  
  public String print() { return "(? " + test.print() + " " + conseq.print() + " " + alt.print() + ")"; }
}

/* This class is essentially a copy of class If but with more accurate typing to simplify IfForm computations. */
class IfIf implements IfForm {
  
  private IfForm test,  conseq,alt;
  
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
  
  public IfForm normalize() { ... }

  /** Given that this is already normalized, test must be atomic. */
  public IfForm headNormalize(IfForm c, IfForm a) { ... }
  
  /** Evaluate normalized this in Environment env. */
  public IfForm eval(Environment env) { ... }
  
  public Form convertToBool() { ... }
                  
  public String print() { return "(? " + test().print() + " " + conseq().print() + " " + alt().print() + ")"; }
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


  
