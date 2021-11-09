;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname boolsimp.rkt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Given
(define-struct Not (arg))
(define-struct And (left right))
(define-struct Or (left right))
(define-struct Implies (left right))
(define-struct If (test conseq alt))
; a boolExp is either
; * a constant true or false,
; * a variable S,
; * a Not form (make-Not X),
; * an And form (make-And X Y),
; * an Or form (make-Or X Y),
; * an Implies form (make-Implies X Y),
; * an If form (make-If X Y Z),
; where S is a symbol (other than a keyword) and X, Y, and Z are boolExps.

; Template for boolExp
#|
(define (be-fun ... be ...)
  (cond [(equal? be true] ...]
        [(equal? be false] ...]
        [(symbol? be) ...]
        [(Not? be) ... (be-fun ... (Not-arg be) ...) ...]
        [(And? be) ... (be-fun ... (And-left be) ...)
                  ... (be-fun ... (And-right be) ...) ...]
        [(Or? be)  ... (be-fun ... (Or-left be) ...)
                  ... (be-fun ... (Or-right be) ...) ...]
        [(Implies? be) ... (be-fun ... (Implies-left be) ...)
                      ... (be-fun ... (Implies-right be) ...) ...]
        [else ;; (must be (make-If be1 be2 be3))
         ... (be-fun ... (If-test be) ...)
         ... (be-fun ... (If-conseq be) ...) 
         ... (be-fun ... (If-alt be) ...) ...]))
|# 

; An ifExp is either:
; * a variable S, or
; * an ifForm (make-If I1 I2 I3)
; where S is a symbol and I1, I2, and I3 are ifExps.

; Template for ifExp
#|
(define (ie-fun ... ie ...)
  (cond [(equal? ie true) ...]
        [(equal? ie false) ...]
        [(symbol? ie) ...]
        [else ;; (must be (make-If? ie1 ie2 ie3))
         ... (be-fun ... (If-test ie) ...)
         ... (be-fun ... (If-conseq ie) ...) 
         ... (be-fun ... (If-alt ie) ...) ...]))

|# 

; convert-to-if: boolExp -> ifExp
; Purpose: (convert-to-if b) converts b to a logically equivalent ifExp.
; Examples
(check-expect (convert-to-if true) true)
(check-expect (convert-to-if false) false)
(check-expect (convert-to-if 'x) 'x)
(check-expect (convert-to-if (make-Not 'x)) (make-If 'x false true))
(check-expect (convert-to-if (make-And 'x 'y)) (make-If 'x 'y false))
(check-expect (convert-to-if (make-Or 'x 'y)) (make-If 'x true 'y))
(check-expect (convert-to-if (make-Implies 'x 'y)) (make-If 'x 'y true))
(check-expect (convert-to-if (make-If 'x 'y 'z)) (make-If 'x 'y 'z))
(check-expect (convert-to-if (make-Not (make-Implies 'x 'y))) (make-If (make-If 'x 'y true) false true))
(check-expect (convert-to-if (make-And (make-Not 'x) (make-Or 'x 'y))) (make-If (make-If 'x false true) (make-If 'x true 'y) false))
(check-expect (convert-to-if (make-Or (make-Or 'x 'y) (make-Not 'y))) (make-If (make-If 'x true 'y) true (make-If 'y false true)))
(check-expect (convert-to-if (make-Implies (make-Not 'x) (make-Or 'x 'y))) (make-If (make-If 'x false true) (make-If 'x true 'y) true))
(check-expect (convert-to-if (make-If (make-Not 'x) (make-Or 'x 'y) (make-And 'x 'y))) (make-If (make-If 'x false true) (make-If 'x true 'y) (make-If 'x 'y false)))

;; Template Instantiation
; Template for boolExp
#|
(define (convertToIf be)
  (cond [(boolean? be ) ...]  ;; the return value for true and false is parametric in be
        [(symbol? be) ...]
        [(Not? be) ... (convertToIf (Not-arg be)) ...]
        [(And? be) ... (convertToIf (And-left be)) ... (convertToIf (And-right be)) ...]
        [(Or? be)  ... (convertToIf (Or-left be)) ... (convertToIf (Or-right be)) ...]
        [(Implies? be) ... (convertToIf (Implies-left be)) ... (convertToIf (Implies-right be)) ...]
        [else ;; (must be (make-If be1 be2 be3))
         ... (convertToIf (If-test be)) ... (convertToIf (If-conseq be)) ... (convertToIf (If-alt be)) ...]))
|#
;; Code
(define (convert-to-if b)
  (cond [(boolean? b) b] 
        [(symbol? b) b]
        [(Not? b) (make-If (convert-to-if (Not-arg b)) false true)]
        [(And? b) (make-If (convert-to-if (And-left b)) (convert-to-if (And-right b)) false)]
        [(Or? b)  (make-If (convert-to-if (Or-left b)) true (convert-to-if (Or-right b)))]
        [(Implies? b) (make-If (convert-to-if (Implies-left b)) (convert-to-if (Implies-right b)) true)]
        [else (make-If (convert-to-if (If-test b)) (convert-to-if (If-conseq b)) (convert-to-if (If-alt b)))]))

; An ifExp ie is a normalized-ifExp iff (i) ie is a variable (symbol); or (ii) an ifForm (make-if X Y Z) 
; where X is a constant or a variable and Y and Z are normalized-ifExps.

; normalize: ifExp -> normalized-ifExp
; Purpose: (normalize ie) returns an equivalent ifExp where If forms do not appear in test position. 

; Examples
(check-expect (normalize true) true)
(check-expect (normalize false) false)
(check-expect (normalize 'x) 'x)
(check-expect (normalize (make-If 'x 'y 'z)) (make-If 'x 'y 'z))
(check-expect (normalize (make-If (make-If 'x 'y 'z) 'u 'v)) (make-If 'x (make-If 'y 'u 'v) (make-If 'z 'u 'v)))

; Template Instantiation
#|
(define (normalize ie)
  (cond [(equal? ie true) ...]
        [(equal? ie false) ...]
        [(symbol? ie) ...]
        [else ;; (must be (make-If? ie1 ie2 ie3))
         ... (normalize (If-test ie))
         ... (normalize (If-conseq ie)) 
         ... (normalize (If-alt ie)) ...]))
|#

; Code
(define (normalize ie)
  (cond [(boolean? ie) ie]
        [(symbol? ie)  ie]
        [else ;; (must be (make-If? ie1 ie2 ie3))
         (head-normalize (normalize (If-test ie)) (normalize (If-conseq ie)) (normalize (If-alt ie)))]))

; normalized-ifExp normalized-ifExp normalize-ifExp -> normalized-ifExp
; Purpose: (normalized-ifExp x y z) returns a normalized ifExp equivalent to (make-If x y z)

; Examples
(check-expect (head-normalize 'x 'y 'z) (make-If 'x 'y 'z))
(check-expect (head-normalize true 'y 'z) (make-If true 'y 'z))
(check-expect (head-normalize false 'y 'z) (make-If false 'y 'z))
(check-expect (head-normalize (make-If 'x 'y 'z) 'u 'v) (make-If 'x (make-If 'y 'u 'v) (make-If 'z 'u 'v)))
(check-expect (head-normalize (make-If 'x (make-If 'yt 'yc 'ya) (make-If 'zt 'zc 'za)) 'u 'v) 
              (make-If 'x (make-If 'yt (make-If 'yc 'u 'v) (make-If 'ya 'u 'v)) (make-If 'zt (make-If 'zc 'u 'v) (make-If 'za 'u 'v))))

; Template Instantiation
#|
(define (head-normalize test alt conseq)
  (cond [(equal? test true) ...]
        [(equal? test false) ...]
        [(symbol? test) ...]
        [else ;; (must be (make-If? ie1 ie2 ie3))
         ... (head-normalize (If-test test) .. ..)
         ... (head-normalize (If-conseq test) .. ..) 
         ... (head-normalize (If-alt test) .. ..) ...]))
|#
;; Code
(define (head-normalize test conseq alt)
  (cond [(boolean? test)  (make-If test conseq alt)]
        [(symbol? test)   (make-If test conseq alt)]
        [else ;; (test must be (make-If? ie1 ie2 ie3))
           (make-If (If-test test) (head-normalize (If-conseq test) conseq alt) (head-normalize (If-alt test) conseq alt))]))

; Given
(define-struct binding (var val))
#|

A (bindingOf T) is a structure (make-binding s v) where s is a symbol and v is value of type T.

An (environmentOf T) is a (listOf (bindingOf T)).

An (option-bidingOf T) is either:
* empty, or
* a (bindingOf T) b.
|#

; Constant definition
(define empty-env empty)

;; Example bindings and environments for subsequent use
(define b2 (make-binding 'b true))
(define b1 (make-binding 'a false))
(define env2 (list b2))
(define env1 (cons b1 env2))


;; extend: (environmentOf T) symbol T -> (environmentOf T)
;; Purpose: (extend e s v) constructs an environment consisting of e with the new binding of s to v.  If e already contains a binding for s, it is superseded.
;; Template Instantiation: trivial [no recursion or case splitting]
;; Code
(define (extend env var val) (cons (make-binding var val) env))

;; lookup: symbol (environmentOf T) -> (option-bindingOf T)
;; Purpose: (lookup s env) returns the first binding b in env such that (equal? (binding-var b) s).  If no such binding
;;          exists, (lookup s env) returns empty.

;; Examples:

(check-expect (lookup 'x empty) empty)
(check-expect (lookup 'x env2) empty)
(check-expect (lookup 'b env2) b2)
(check-expect (lookup 'b env1) b2)
(check-expect (lookup 'c env1) empty)
(check-expect (lookup 'a env1) b1)

#|
Template instantiation:
(define (lookup s env)
  (cond [(empty env) ...]
        [else ... (first env) ... (lookup s (rest env)) .. .]))
|#

(define (lookup s env)
  (cond [(empty? env) empty]
        [else 
         (local ((define b (first env)))
                (if (equal? s (binding-var b)) b (lookup s (rest env))))]))

;; eval: norm-ifExp (environmentOf boolean) -> norm-ifExp
;; Purpose: (eval ie env) reduces ie to "simplest form" given the bindins in env.

;; Examples
(check-expect (eval true empty-env) true)
(check-expect (eval false empty-env) false)
(check-expect (eval 'x empty-env) 'x)
(check-expect (eval 'x (list (make-binding 'x true))) true)
(check-expect (eval (make-If 'x 'x true) empty-env) true)
(check-expect (eval (make-If 'x (make-If 'z true 'x) (make-If 'z true (make-If 'x false true))) empty-env) true)

;; Template Instantiation
#|
(define (eval ie env)
  (cond [(equal? ie true) ...]
        [(equal? ie false) ...]
        [(symbol? ie) ...]
        [else ;; (ie must be (make-If? ie1 ie2 ie3))
         ... (eval (If-test test) .. ..)
         ... (eval (If-conseq test) .. ..) 
         ... (eval (If-alt test) .. ..) ...]))
|#
;; Code
(define (eval ie env)
  (cond [(equal? ie true) true]
        [(equal? ie false) false]
        [(symbol? ie) 
         (local [(define b (lookup ie env))]
           (if (empty? b) ie (binding-val b)))]
        [else 
         (local [(define test (eval (If-test ie) env))
                 (define conseq (If-conseq ie))
                 (define alt (If-alt ie))]
           (cond 
             [(equal? test true) (eval conseq env)]
             [(equal? test false) (eval alt env)]
             ;; test is a variable
;;             [(equal? conseq alt) test]
             [else
              (local [(define new-conseq (eval conseq (extend env test true)))
                      (define new-alt (eval alt (extend env test false)))]
                (cond [(equal? new-conseq new-alt) new-conseq]
                      [(and (equal? new-conseq true) (equal? new-alt false)) test]
                      [else (make-If test new-conseq new-alt)]))]))]))
         
;; convert-to-bool: norm-ifExp -> boolExp
;; Purpose: converts a simplified, normalized ifExp back to a conventional boolean expression
;; Examples

;; Template Instantiation
#|
(define (convert-to-bool ie)
  (cond [(equal? ie true) ...]
        [(equal? ie false) ...]
        [(symbol? ie) ...]
        [else ;; (must be (make-If? ie1 ie2 ie3))
         ... (convert-to-if (If-test ie) .. ..)
         ... (convert-to-if (If-conseq ie) .. ..) 
         ... (convert-to-if (If-alt ie) .. ..) ...]))
|#

;; Code
(define (convert-to-bool ie)
  (cond [(boolean? ie) ie]
        [(symbol? ie) ie]
        [else ;; (must be (make-If? v ie2 ie3)) where v is a variable and ie2 and ie3 and normalized and simplified
         (local [(define test (If-test ie))
                 (define conseq (convert-to-bool (If-conseq ie)))
                 (define alt (convert-to-bool (If-alt ie)))]
           (cond [(and (equal? conseq false) (equal? alt true)) (make-Not test)]
                 [(false? alt) (make-And test conseq)]
                 [(equal? alt true) (make-Implies test conseq)]
                 [(equal? conseq true) (make-Or test alt)]
                 [else (make-If test conseq alt)]))]))

(check-expect (convert-to-bool true) true)
(check-expect (convert-to-bool false) false)
(check-expect (convert-to-bool (make-If 'x false true)) (make-Not 'x))
(check-expect (convert-to-bool (make-If 'x 'y false)) (make-And 'x 'y))
(check-expect (convert-to-bool (make-If 'x true 'y)) (make-Or 'x 'y))
(check-expect (convert-to-bool (make-If 'x 'y true)) (make-Implies 'x 'y))
(check-expect (convert-to-bool (make-If 'x 'y 'z)) (make-If 'x 'y 'z))

(check-expect (convert-to-bool (make-If 'a (make-If 'b 'c (make-If 'f true 'z)) false))
              (make-And 'a (make-If 'b 'c (make-Or 'f 'z))))

(define (raw-reduce be) 
  (convert-to-bool (eval (normalize (convert-to-if be)) empty-env)))

(check-expect 
 (raw-reduce (make-Or (make-And 'x 'y) (make-Or (make-And 'x (make-Not 'y)) (make-Or (make-And (make-Not 'x) 'y) (make-And (make-Not 'x) (make-Not 'y)))))) 
 true)

(check-expect 
 (raw-reduce (make-Or (make-And 'x (make-Not 'y)) (make-Or (make-And (make-Not 'x) 'y) (make-And (make-Not 'x) (make-Not 'y))))) 
 (make-Implies 'x (make-Not 'y)))
 
;;A bool-SchemeExp is either:
;;* a boolean constant true or false;
;;* a symbol;
;;* (list 'not X) where X is a bool-SchemeExp;
;;* (list op X Y) where op is 'and 'or 'implies where X and Y are bool-SchemeExps;
;;* (list 'if X Y Z) where X, Y, and Z are bool-SchemeExps

;;parse: bool-SchemeExp -> boolExp
;;Purpose: (parse bse) returns the boolExp corresponding to the bool-SchemeExp bse

;;Examples
(check-expect (parse 'T) true)
(check-expect (parse 'F) false)
(check-expect (parse 'x) 'x)
(check-expect (parse '(& x y)) (make-And 'x 'y))
(check-expect (parse '(\| x y)) (make-Or 'x 'y))
(check-expect (parse '(> x y)) (make-Implies 'x 'y))
(check-expect (parse '(? x y z)) (make-If 'x 'y 'z))

                                        
;; Template instantiation
#|
(define (parse bse)
  (cond [(equal? bse 'T) ...]
        [(equal? bse 'F) ...]
        [(symbol? bse) ...]
        [else 
         (local [(define head (first bse))]
           (cond [(equal? head '!) ...]
                 [(equal? head '&) ...]
                 [(equal? head '\|) ...]
                 [(equal? head '>) ...]
                 [(equal? head '?) ...]))]))
|#
(define (parse bse)
  (cond [(equal? bse 'T) true]
        [(equal? bse 'F) false]
        [(symbol? bse) bse]
        [else 
         (local [(define head (first bse))
                 (define args (rest bse))
                 (define val1 (parse (first args)))]
           (cond [(equal? head '!) (make-Not val1)]
                 [else 
                  (local [(define val2 (parse (first (rest args))))]
                    (cond [(equal? head '&) (make-And val1 val2)]
                          [(equal? head '\|) (make-Or val1 val2)]
                          [(equal? head '>) (make-Implies val1 val2)]
                          [(equal? head '?) (make-If val1 val2 (parse (first (rest (rest args)))))]))]))]))

;;parse: boolExp -> bool-SchemeExp
;;Purpose: (parse bse) returns the boolExp corresponding to the bool-SchemeExp bse

;; Examples
(check-expect (unparse 'T) 'T)
(check-expect (unparse 'F) 'F)
(check-expect (unparse 'x) 'x)
(check-expect (unparse (make-Not 'x)) '(! x))
(check-expect (unparse (make-And 'x 'y)) '(& x y))
(check-expect (unparse (make-Or 'x 'y)) '(\| x y))
(check-expect (unparse (make-Implies 'x 'y)) '(> x y))
(check-expect (unparse (make-If 'x 'y 'z)) '(? x y z))
           
;; Template Instantiation
#|
(define (unparse be)
  (cond [(equal? be true) ...]
        [(equal? be false) ...]
        [(symbol? be) ...]
        [(Not? be) ... (unparse (Not-arg be)) ...]
        [(And? be) ... (unparse (And-left be))
                  ... (unparse (And-right be)) ...]
        [(Or? be) ... (unparse (Or-left be))
                  ... (unparse (Or-right be)) ...]
        [(Implies? be) ... (unparse (Implies-left be))
                       ... (unparse (Implies-right be)) ...]
        [else ;; (must be (make-If be1 be2 be3))
         ... (unparse (If-test be))
         ... (unparse (If-conseq be)) 
         ... (unparse (If-alt be)) ...]
|#
(define (unparse be)
  (cond [(boolean? be) (if be 'T 'F)]
        [(symbol? be) be]
        [(Not? be) (list '! (unparse (Not-arg be)))]
        [(And? be) (list '& (unparse (And-left be)) (unparse (And-right be)))]
        [(Or? be)  (list '\|  (unparse (Or-left be))  (unparse (Or-right be)))]
        [(Implies? be) (list '> (unparse (Implies-left be)) (unparse (Implies-right be)))]
        [(If? be) (list '? (unparse (If-test be)) (unparse (If-conseq be)) (unparse (If-alt be)))]))

;; reduce: bool-SchemeExp -> bool-SchemeExp
;; Purpose: (reduce bse) reduces bse to simplest form

;; Examples
(check-expect (reduce '(\| (& x y) (\| (& x (! y)) (\| (& (! x) y) (& (! x) (! y)))))) 'T)
(check-expect (reduce '(\| (& x (! y)) (\| (& (! x) y) (& (! x) (! y))))) '(> x (! y)))
(check-expect (reduce '(\| (& x (& y z)) (\| (& x (& y (! z))) (\| (& x (& (! y) z)) (\| (& x (& (! y) (! z))) (\| (& (! x) (& y z)) (\| (& (! x) (& y (! z))) (& (! x) (& (! y) z))))))))) '(\| x (\| y z)))

;; Code
(define (reduce bse) (unparse (raw-reduce (parse bse))))