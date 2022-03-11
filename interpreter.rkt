#lang plait
;; See https://docs.racket-lang.org/plait/
;; Adapted by S. Rivoire from https://cs.brown.edu/courses/cs173/2021/implementation-specs/interp/interpreter.rkt

;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (desugar (parse str))))

;; DO NOT EDIT ABOVE THIS LINE =================================================

;;Edited by Zachary Robinson

(define (desugar [expr : Expr]): Expr
  (type-case Expr expr
  [(e-num value) (e-num value)];numbers
  [(e-str value) (e-str value)];strings
  [(e-bool value)(e-bool value)];bools
  [(e-op op l r) (e-op op (desugar l) (desugar r))];operators
  [(e-if c consq alt) (e-if (desugar c) consq alt)];if statements
  [(sugar-and l r) (e-if (desugar l)
                         (e-if (desugar r);left is true
                               (e-bool #t);both are true
                               (e-bool #f));right is false
                         (e-bool #f))];left is false
  [(sugar-or l r) (e-if (desugar l)
                        (e-bool #t);left is true
                        (e-if (desugar r);left is false
                              (e-bool #t);right is true
                              (e-bool #f)))];both are false
  [else expr]
  )
 )
  

(define empty-Env (hash empty))                                  ;helper alias to create an empty list

(define (insert-Env [env : Env][sym : Symbol][val : Value]): Env ;helper function to insert a key value pair into an Env
  (hash-set env sym val)
  )

(define (lookup-Env [env : Env][sym : Symbol]): Value            ;helper function to lookup a key in an Env
  (type-case (Optionof Value) (hash-ref env sym)
    [(some s) s]                       ;entry found
    [(none) (v-str "Entry not found")]);entry not found
  )

(define (interp [expr : Expr]): Value
  (interpEnv expr empty-Env)
  )


(define (interpEnv [expr : Expr][env : Env]): Value
  (type-case Expr expr
    [(e-num value) (v-num value)];numbers
    [(e-str value) (v-str value)];strings
    [(e-bool value)(v-bool value)];bools
    
    [(e-op op l r)
     (let* ([l (interpEnv l env)])(let* ([r (interpEnv r env)])(cond          ;operations, this condition determines what type of operation will be performed
                     [(op-plus? op)(if (v-num? l)             ;check if the left type is correct
                                       (if (v-num? r)           ;check if the right type is correct
                                         (v-num(+ (v-num-value l) (v-num-value r)));if so, add l and r evaluated recursively
                                         (raise-error (err-bad-arg-to-op op r)))     ;if not, raise error
                                       (raise-error (err-bad-arg-to-op op l))        ;if not, raise error
                                       )]
                     
                     [(op-append? op)(if (v-str? l)           ;check if the left type is correct
                                       (if (v-str? r)           ;check if the right type is correct
                                         (v-str(string-append (v-str-value l) (v-str-value r)));if so, append l and r evaluated recursively
                                         (raise-error (err-bad-arg-to-op op r)))        ;if not, raise error
                                       (raise-error (err-bad-arg-to-op op l))        ;if not, raise error
                                       )]

                     [(op-str-eq? op)(if (v-str? l)             ;check if the left type is correct
                                       (if (v-str? r)           ;check if the right type is correct
                                         (v-bool(string=? (v-str-value l) (v-str-value r)));if so, evaluate if l and r evaluated recursively are equal
                                         (raise-error (err-bad-arg-to-op op r)))        ;if not, raise error
                                       (raise-error (err-bad-arg-to-op op l))        ;if not, raise error
                                       )]

                     [(op-num-eq? op)(if (v-num? l)             ;check if the left type is correct
                                       (if (v-num? r)            ;check if the right type is correct
                                         (v-bool(eq? (v-num-value l) (v-num-value r)));if so, evaluate if l and r evaluated recursively are equal
                                         (raise-error (err-bad-arg-to-op op r)))        ;if not, raise error
                                       (raise-error (err-bad-arg-to-op op l))        ;if not, raise error
                                       )]
                     )))]
    [(e-if c consq alt)(let* ([c (interpEnv c env)])(if (v-bool? c)                                       ;check if the type is correct
                          (if (v-bool-value c)                                 ;if so, check if the recursively evaluated c is true or false
                           (interpEnv consq env)                                                ;if,so recursively evaluate consq
                           (interpEnv alt env))                                                 ;if not, recursively evaluate alt
                          ((raise-error (err-if-got-non-boolean c)))))];if not, raise error
    [else (v-str "you weren't supposed to do that")];catches undefined types
 ))