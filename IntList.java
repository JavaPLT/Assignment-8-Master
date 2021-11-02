/** The functional list type defined by IntList := EmptyIntList + ConsIntList(int, IntList) */
abstract class IntList {

  /** Visitor hook */
  abstract public Object visit(IntListVisitor iv);
  
  /** Adds i to the front of this. */
  public ConsIntList cons(int i) { return new ConsIntList(i, this); }
  /** Returns the unique empty list. */
  public static EmptyIntList empty() { return EmptyIntList.ONLY; }
  
  /** Utility functions for testing */
  static IntList countDown(int n) {
    if (n <= 0) return IntList.empty();
    else return countDown(n-1).cons(n);
  }
  static IntList countUpHelp(int m, int n) {
    if (m > n) return IntList.empty();
    else return countUpHelp(m+1,n).cons(m);
  }
  static IntList countUp(int n) { return countUpHelp(1,n); }
  
  /** Sorts this into ascending order. */
  abstract public IntList sort();
  /** Assuming this is sorted, return a list that inserts i into sorted position in this. */
  abstract public ConsIntList insert(int i);
  
  /* Overriding equals to implement structural equality */
  abstract public boolean equals(Object other);
    
  /* The Intlist composite type overrides toString to provide a List-like string represenation. */
  
  /** A help function for producing a Lisp-like representation for IntLlist. Called by toString() and toStringHelp() */
  abstract String toStringHelp();
}
 
/** Concrete empty list structure containing nothing. */
class EmptyIntList extends IntList {
  
  /** Singleton binding */
  public static final EmptyIntList ONLY = new EmptyIntList();
  
  /** Single pattern constructor */
  private EmptyIntList() { }
  
  /** Visitor hook */
  public Object visit(IntListVisitor v) { return  v.forEmptyIntList(this); }
  
  public IntList sort() { return ONLY; }

  public ConsIntList insert(int i) { return cons(i); }
  
  public boolean equals(Object o) { return o == ONLY; }
  
  /** Constructs a Lisp-like String representation. */
  public String toString() { return "()"; }
 
  String toStringHelp() { return ")"; }
}
 
/** Concrete non-empty list structure containing an int, called first, and an IntList called rest. */
class ConsIntList extends IntList {
  
  /** Cons node fields */
  int first;
  IntList rest;
  
  /** Data constructor for ConsIntList */
  ConsIntList(int f, IntList r) { first = f; rest = r; }
  
  /** Accessors */
  public int first() { return first; }
  public IntList rest() { return rest; }
  
  public Object visit(IntListVisitor v) { return v.forConsIntList(this); }
  
  public IntList sort() { return rest.sort().insert(first); }
  public ConsIntList insert(int i) {
    if (i <= first) { return cons(i); }
    else { return rest.insert(i).cons(first); }
  }
  
  public boolean equals(Object o) { 
    if (o.getClass() != ConsIntList.class) return false;
    ConsIntList other = (ConsIntList) o;  // never fails
    return (other.first() == first) && (other.rest().equals(rest));
  }
  
  /** Constructs a Lisp-like String representation. */
  public String toString() { return "(" + first + rest.toStringHelp(); }
  public String toStringHelp() { return " " + first + rest.toStringHelp(); }
}

  