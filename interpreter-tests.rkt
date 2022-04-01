#lang racket
;; Adapted by S. Rivoire from https://cs.brown.edu/courses/cs173/2021/implementation-specs/interp/interpreter-tests.rkt
;; my tests for SSU CS 460 Programming Assignment #1

;; =============================================================================
;; Interpreter: interpreter-tests.rkt
;; =============================================================================

(require "interpreter.rkt" 
         "support.rkt"
         "test-support.rkt")

;; DO NOT EDIT ABOVE THIS LINE =================================================
(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE ==========
  ;;Edited by Zachary Robinson and Nicholas Keng after line 254

  ;; Tests 1-3: Primitive datatypes
  (test-equal? "1: Num primitive" (eval `201) (v-num 201))
  (test-equal? "2: Str primitive" (eval `"cat") (v-str "cat"))
  (test-equal? "3a: Bool primitive (true)" (eval `true) (v-bool #t))
  (test-equal? "3b: Bool primitive (false)" (eval `false) (v-bool #f))
  

  ;; Test 4: Operators, with primitive operands
  (test-equal? "4a: + with Num primitives" (eval `(+ 201 203)) (v-num 404))
  (test-equal? "4b: ++ with Str primitives" (eval `(++ "cat" "dog")) (v-str "catdog"))
  (test-equal? "4c: str= with Str primitives, yes equal" (eval `(str= "dog" "dog")) (v-bool #t))
  (test-equal? "4d: str= with Str primitives, not equal" (eval `(str= "cat" "dog")) (v-bool #f))
  (test-equal? "4e: num= with Num primitives, yes equal" (eval `(num= 404 404)) (v-bool #t))
  (test-equal? "4f: num= with Num primitives, not equal" (eval `(num= 201 203)) (v-bool #f))

  ;; Test 5: If-expressions with primitive condition, primitive if, primitive else
  (test-equal? "5a: if with primitive operands" (eval `(if true 201 202)) (v-num 201))
  (test-equal? "5b: if with primitive operands" (eval `(if false 201 202)) (v-num 202))
  (test-equal? "5c: if with primitive operands" (eval `(if true "cat" "dog")) (v-str "cat"))
  (test-equal? "5d: if with primitive operands" (eval `(if false "cat" "dog")) (v-str "dog"))
  (test-equal? "5e: if with primitive operands" (eval `(if true false true)) (v-bool #f))
  (test-equal? "5f: if with primitive operands" (eval `(if false false true)) (v-bool #t))

  ;; Test 6: Operators, with recursively evaluated operands
  ; +
  (test-equal? "6a: + with left op gently recursive" (eval `(+ (+ 69 94) 203)) (v-num 366))
  (test-equal? "6b: + with left op deeply recursive" (eval `(+ (+ (+ 69 94) 22) 203)) (v-num 388))
  (test-equal? "6c: + with right op gently recursive" (eval `(+ 201 (+ 69 94))) (v-num 364))
  (test-equal? "6d: + with right op deeply recursive" (eval `(+ 201 (+ (+ 69 94) 22))) (v-num 386))
  (test-equal? "6e: + with both ops gently recursive" (eval `(+ (+ 69 94) (+ 69 94))) (v-num 326))
  (test-equal? "6f: + with both ops deeply recursive"
               (eval `(+ (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 774))

  ; ++
  (test-equal? "6g ++ with left op gently recursive"
               (eval `(++ (++ "sonoma " "state ") "university")) (v-str "sonoma state university"))
  (test-equal? "6h ++ with left op deeply recursive"
               (eval `(++ (++ (++ "sonoma " "state ") "university ") "california"))
               (v-str "sonoma state university california"))
  (test-equal? "6i ++ with right op gently recursive"
               (eval `(++ "university " (++ "sonoma " "state")))
               (v-str "university sonoma state"))
  (test-equal? "6j ++ with right op deeply recursive"
               (eval `(++ "university " (++ (++ "sonoma " "state ") "california")))
               (v-str "university sonoma state california"))
  (test-equal? "6k ++ with both ops gently recursive"
               (eval `(++ (++ "sonoma " "state") (++ "sonoma " "state")))
               (v-str "sonoma statesonoma state"))
  (test-equal? "6l ++ with both ops deeply recursive"
               (eval `(++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california")))
               (v-str "californiasonoma state universitysonoma state universitycalifornia"))

  ; num=
  (test-equal? "6m: num= with left op gently recursive (true)"
               (eval `(num= (+ 69 94) 163)) (v-bool #t))
  (test-equal? "6n: num= with left op gently recursive (false)"
               (eval `(num= (+ 69 94) 203)) (v-bool #f))
  (test-equal? "6o: num= with left op deeply recursive (true)"
               (eval `(num= (+ (+ (+ 69 94) 22) 203) 388)) (v-bool #t))
  (test-equal? "6p: num= with left op deeply recursive (false)"
               (eval `(num= (+ (+ (+ 69 94) 22) 203) 400)) (v-bool #f))
  (test-equal? "6q: num= with right op gently recursive (true)"
               (eval `(num= 163 (+ 69 94))) (v-bool #t))
  (test-equal? "6r: num= with right op gently recursive (false)"
               (eval `(num= 203 (+ 69 94))) (v-bool #f))
  (test-equal? "6s: num= with right op deeply recursive (true)"
               (eval `(num= 386 (+ 201 (+ (+ 69 94) 22)))) (v-bool #t))
  (test-equal? "6t: num= with right op deeply recursive (false)"
               (eval `(num= 400 (+ 201 (+ (+ 69 94) 22)))) (v-bool #f))
  (test-equal? "6u: num= with both ops gently recursive (true)"
               (eval `(num= (+ 69 94) (+ 69 94))) (v-bool #t))
  (test-equal? "6v: num= with both ops gently recursive (false)"
               (eval `(num= (+ 69 94) (+ 69 95))) (v-bool #f))
  (test-equal? "6w: num= with both ops deeply recursive (true)"
               (eval `(num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 201))) (v-bool #t))
  (test-equal? "6x: num= with both ops deeply recursive (false)"
               (eval `(num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-bool #f))

  ; str=
  (test-equal? "6y: str= with left op gently recursive (true)"
               (eval `(str= (++ "sonoma " "state") "sonoma state")) (v-bool #t))
  (test-equal? "6z: str= with left op gently recursive (false)"
                (eval `(str= (++ "sonoma " "state") "sonomastate"))  (v-bool #f))
  (test-equal? "6aa: str= with left op deeply recursive (true)"
               (eval `(str= (++ (++ (++ "sonoma " "state ") "university ") "california") "sonoma state university california"))
               (v-bool #t))
  (test-equal? "6ab: str= with left op deeply recursive (false)"
               (eval `(str= (++ (++ (++ "sonoma " "state ") "university ") "california") "sonomastate university california"))
               (v-bool #f))
  (test-equal? "6ac: str= with right op gently recursive (true)"
               (eval `(str= "sonoma state" (++ "sonoma " "state"))) (v-bool #t))
  (test-equal? "6ad: str= with right op gently recursive (false)"
               (eval `(str= "sonomastate" (++ "sonoma " "state"))) (v-bool #f))
  (test-equal? "6ae: str= with right op deeply recursive (true)"
               (eval `(str= (++ "university " (++ (++ "sonoma " "state ") "california"))
                            "university sonoma state california"))
               (v-bool #t))
  (test-equal? "6af: str= with right op deeply recursive (false)"
               (eval `(str= (++ "university " (++ (++ "sonoma " "state ") "california"))
                            "university sonomastate california"))
               (v-bool #f))
  (test-equal? "6ag: str= with both ops gently recursive (true)"
               (eval `(str= (++ "sonoma " "state") (++ "sonoma " "state"))) (v-bool #t))
  (test-equal? "6ah: str= with both ops gently recursive (false)"
               (eval `(str= (++ "sonoma " "state") (++ "sonoma" "state"))) (v-bool #f))
  (test-equal? "6ai: str= with both ops deeply recursive (true)"
               (eval `(str= (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ "california" (++ (++ "sonoma " "state ") "university"))))
               (v-bool #t))
  (test-equal? "6aj: str= with both ops deeply recursive (false)"
               (eval `(str= (++ "california " (++ (++ "sonoma " "state ") "university"))
                          (++ "california" (++ (++ "sonoma " "state ") "university"))))
               (v-bool #f))
  
  ; if
  (test-equal? "6ak: if with true condition and gently recursive consq clause"
               (eval `(if true (+ 69 94) 202)) (v-num 163))
  (test-equal? "6al: if with false condition and gently recursive consq clause"
               (eval `(if false (+ 69 94) 202)) (v-num 202))
  (test-equal? "6am: if with true condition and deeply recursive consq clause"
               (eval `(if true (+ (+ (+ 69 94) 22) 203) 202)) (v-num 388))  
  (test-equal? "6an: if with false condition and deeply recursive consq clause"
               (eval `(if false (+ (+ (+ 69 94) 22) 203) 202)) (v-num 202))  
  (test-equal? "6ao: if with true condition and gently recursive alt clause"
               (eval `(if true "SSU" (++ "sonoma " "state"))) (v-str "SSU"))
  (test-equal? "6ap if with false condition and gently recursive alt clause"
               (eval `(if false "SSU" (++ "sonoma " "state"))) (v-str "sonoma state"))
  (test-equal? "6aq: if with true condition and deeply recursive alt clause"
               (eval `(if true "SSU" (++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california")))) (v-str "SSU")) 
  (test-equal? "6ar: if with false condition and deeply recursive alt clause"
               (eval `(if false "SSU" (++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california"))))
               (v-str "californiasonoma state universitysonoma state universitycalifornia"))
  (test-equal? "6as: if with true condition and both clauses gently recursive"
               (eval `(if true (+ 69 94) (+ 201 203))) (v-num 163))
  (test-equal? "6at: if with false condition and both clauses gently recursive"
               (eval `(if false (+ 69 94) (+ 201 203))) (v-num 404))
  (test-equal? "6au: if with true condition and both clauses deeply recursive"
               (eval `(if true (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 386))
  (test-equal? "6av: if with false condition and both clauses deeply recursive"
               (eval `(if false (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 388))
  (test-equal? "6aw: if with gently recursive true condition"
               (eval `(if (str= "dog" "dog") 201 202)) (v-num 201))
  (test-equal? "6ax: if with gently recursive false condition"
               (eval `(if (str= "dog" "cat") 201 202)) (v-num 202))
  (test-equal? "6ay: if with deeply recursive true condition"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 201)) 201 202))
               (v-num 201))
  (test-equal? "6az: if with deeply recursive false condition"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203)) 201 202))
               (v-num 202))
  (test-equal? "6ba: if with gently recursive true condition and gently recursive clauses"
               (eval `(if (str= "dog" "dog") (++ "sonoma " "state") (++ "state " "sonoma")))
               (v-str "sonoma state"))
  (test-equal? "6bb: if with gently recursive false condition and gently recursive clauses"
               (eval `(if (str= "dog" "cat") (++ "sonoma " "state") (++ "state " "sonoma")))
               (v-str "state sonoma"))
  (test-equal? "6bc: if with deeply recursive true condition and deeply recursive clauses"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22))
                                (+ 201 (+ (+ 69 94) 22)))
                                (+ (+ (+ 69 94) 22) 201) (+ (+ (+ 69 94) 22) 203)))
                     (v-num 386))
  (test-equal? "6bd: if with deeply recursive false condition and deeply recursive clauses"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22))
                                (+ 203 (+ (+ 69 94) 22)))
                                (+ (+ (+ 69 94) 22) 201) (+ (+ (+ 69 94) 22) 203)))
                     (v-num 388))
  
  ;; Test 7: The coolest tests from Piazza
  ; 7a: Credit: Jonathan Calderon Chavez
  (test-equal? "7a: (++ <expr> <expr>) - nested append"
               (eval `{++ (++(++"g" "o")(++ "o" (++ "f" "y ")))(++(++ "g" (++ "o" "o"))(++ "b" (++ "e" "r")))})
               (v-str "goofy goober"))
   ;7b: Credit: David Holstedt
  (test-equal? "7b: Arbitrarily nested statements"
               (eval `{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+ 1 0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0})
               (v-num 1))
  
  ;; Test 8: Error handling for if-got-non-boolean
  (test-raises-interp-error? "8a: String constant in if-condition"
                             (eval `(if "rat" "cat" "dog"))
                             (err-if-got-non-boolean (v-str "rat")))
  (test-raises-interp-error? "8b: More complicated non-bool expr in if-condition"
                             (eval `(if (+ 100 200) "cat" "dog"))
                             (err-if-got-non-boolean (v-num 300)))

  ;; Test 9: Error handling for bad-arg-to-op
  (test-raises-interp-error? "9a: String const as left operand of +"
                             (eval `(+ "cat" 1))
                             (err-bad-arg-to-op (op-plus) (v-str "cat")))
  
  (test-raises-interp-error? "9b: String const as right operand of +"
                             (eval `(+ 1 "cat"))
                             (err-bad-arg-to-op (op-plus) (v-str "cat")))

  (test-raises-interp-error? "9c: Two bad args to +, pick correct one"
                             (eval `(+ "dog" "cat"))
                             (err-bad-arg-to-op (op-plus) (v-str "dog")))
  
  (test-raises-interp-error? "9d: Numeric const as left operand of ++"
                             (eval `(++ 1 "cat"))
                             (err-bad-arg-to-op (op-append) (v-num 1)))
  
  (test-raises-interp-error? "9e: Numeric const as right operand of +"
                             (eval `(++ "cat" 11))
                             (err-bad-arg-to-op (op-append) (v-num 11)))

  (test-raises-interp-error? "9f: Two bad args to ++, pick correct one"
                             (eval `(++ 12 false))
                             (err-bad-arg-to-op (op-append) (v-num 12)))

  (test-raises-interp-error? "9g: Numeric const as left operand of str="
                             (eval `(str= 1 "cat"))
                             (err-bad-arg-to-op (op-str-eq) (v-num 1)))
  
  (test-raises-interp-error? "9h: Numeric const as right operand of str="
                             (eval `(str= "cat" 11))
                             (err-bad-arg-to-op (op-str-eq) (v-num 11)))

  (test-raises-interp-error? "9i: Two bad args to str=, pick correct one"
                             (eval `(str= 12 false))
                             (err-bad-arg-to-op (op-str-eq) (v-num 12)))

  (test-raises-interp-error? "9j: String const as left operand of num="
                             (eval `(num= "cat" 1))
                             (err-bad-arg-to-op (op-num-eq) (v-str "cat")))
  
  (test-raises-interp-error? "9k: String const as right operand of num="
                             (eval `(num= 1 "cat"))
                             (err-bad-arg-to-op (op-num-eq) (v-str "cat")))

  (test-raises-interp-error? "9l: Two bad args to num=, pick correct one"
                             (eval `(num= "dog" "cat"))
                             (err-bad-arg-to-op (op-num-eq) (v-str "dog")))

  ;; New Tests for PA2 =========================================================

  ;; AND and OR tests
   ; - Jesica Wood
  (test-equal? "works with simple desugaring and"
               (eval `(and true true)) (v-bool true))

    (test-equal? "works with desugaring and with num= expression"
               (eval `(and (num= 1 2) true)) (v-bool false))

    (test-equal? "works with desugaring and with str= expression"
               (eval `(and (str= "hi" "hi") true)) (v-bool true))
  
    (test-equal? "works with desugaring and with 2 str= expression"
               (eval `(and (str= "hi" "hi") (str= "i" "hi"))) (v-bool false))

    (test-equal? "works with desugaring and with 2 num= expression"
               (eval `(and (num= 1 1) (num= 100 100))) (v-bool true))
  
    (test-equal? "works with nested AND"
               (eval `(and (and (and (num= 1 1) (str= "p" "p")) true) false)) (v-bool false))

    (test-equal? "works with desugaring or"
               (eval `(or true false)) (v-bool #t))

    (test-equal? "works with desugaring num= or"
               (eval `(or (num= 1 1) false)) (v-bool #t))

    (test-equal? "works with nested OR"
               (eval `(or (or (or (num= 1 1) (str= "p" "p")) true) false)) (v-bool true))
  
   ; - Christopher
  (test-equal? "AND using num= and str="
               (eval `(and (num= 1 1)(str= "hello" "hello")))
               (v-bool #t))
  (test-equal? "AND using str="
               (eval `(and (str= "one" "1")(str= "hello" "hello")))
               (v-bool #f))
   (test-equal? "AND using recursive str= and num="
               (eval `(and (str= (++ "rac" "car") "racecar") (num= (+ 23 54) 56)))
               (v-bool #f))
  (test-equal? "AND using recursion"
               (eval `(and (str= (++ "race" "car") "racecar") (and (num= 1 1) (str= "eq" "eq"))))
               (v-bool #t))
  (test-equal? "OR using num= and str="
               (eval `(or (num= (+ 4 1) (+ 3 2))(str= (++ "cor" "rect") (++ "co" (++ "rre" "ct")))))
               (v-bool #t))
  (test-equal? "OR using num= and str="
               (eval `(or (num= (+ 12 6) (+ 9 7))(str= (++ "inc" "orrect") (++ "in" (++ "cor" "rect")))))
               (v-bool #t))
  (test-equal? "OR using recursion"
               (eval `(or (num= 46 (+ 40 7)) (or (str= "is it" "is it not") (num= 46897 (+ 4671 42226)))))
               (v-bool #t))

  (test-equal? "Test if case for numbers"
               (eval `{ +   1   (if (and (num= 1 1) (num= 2 2)) 1 2)      }) (v-num 2)
  )
  
   ; - Guy Greenleaf
  (test-equal? "Test basic desugar AND"
               (eval `{and true true}) (v-bool #t)
  )

   (test-equal? "Test basic desugar AND returns false "
               (eval `{and true false}) (v-bool #f)
  )
  
  (test-equal? "Test basic desugar OR"
               (eval `{or false true}) (v-bool #t)) 

  
  (test-equal? "Test IF desugar AND returns true"
               (eval `{if (and true true) true false}) (v-bool #t)
  )

  (test-equal? "Test IF desugar OR returns true"
               (eval `{if (or false true) true false}) (v-bool #t)
  ) 
  
  (test-equal? "Test IF desugar AND returns false"
               (eval `{if (and true false) true false}) (v-bool #f)
  )

  (test-equal? "Test IF desugar OR returns false"
               (eval `{if (or false false) true false}) (v-bool #f)
  ) 

  (test-equal? "Test IF desugar AND returns true complex"
               (eval `{if (and (str= (++ "Appending" "This") "AppendingThis") true) true false}) (v-bool #t)
  ) 

  (test-equal? "Test IF desugar OR returns true complex"
               (eval `{if (or false (num= (+ 1 1) 2)) true false}) (v-bool #t)
  ) 
  
  (test-equal? "Test IF desugar AND returns false complex"
               (eval `{if (and true (str= (++ "Appending" "This") "AppendingThisJustKidding" )) true false}) (v-bool #f)
  )

  (test-equal? "Test IF desugar OR returns false complex"
               (eval `{if (or false (num= (+ 1 1) 3)) true false}) (v-bool #f)
  ) 

  (test-equal? "Test IF desugar OR returns true complex SHORT CIRCUIT"
               (eval `{if (or true "abc") true false}) (v-bool #t)
  ) 

  (test-equal? "Test if case for numbers"
               (eval `{+ 1(if (and (num= 1 1) (num= 2 2)) 1 2)}) (v-num 2)
  )

  (test-raises-interp-error? "Test desugar AND bad bool second arg"
               (eval `{and true "abc"}) (err-if-got-non-boolean (v-str "abc"))
  )

  (test-raises-interp-error? "Test desugar AND bad bool first arg"
               (eval `{and "cat" true}) (err-if-got-non-boolean (v-str "cat"))
  ) 

  (test-raises-interp-error? "Test desugar OR bad bool second arg"
               (eval `{or false "abc"}) (err-if-got-non-boolean (v-str "abc"))
  ) 

 (test-raises-interp-error? "Test desugar OR bad bool first arg"
               (eval `{or "cat" true}) (err-if-got-non-boolean (v-str "cat"))
  )
  
  (test-equal? "NESTED TEST: desugar AND - should return true"
               (eval `{if (and (and (and (and true true) (and true true)) (and (and true true) (and true true))) (and (and true true) (and true true))) true false}) (v-bool #t)
  )

  (test-equal? "NESTED TEST: desugar OR - should return true"
               (eval `{if (or (or (or (or false true) (or false true)) (or (or false true) (or false true))) (or (or false true) (or false true))) true false}) (v-bool #t)
  ) 
  
  (test-equal? "NESTED TEST: desugar AND - should return false"
               (eval `{if (and (and (and (and (and true false) (and true false)) (and (and true false) (and true false))) (and (and (and true false) (and true false)) (and (and true false) (and true false)))) (and (and (and true false) (and true false)) (and (and true false) (and true false)))) true false}) (v-bool #f)
  )

  (test-equal? "NESTED TEST: desugar OR - should return false"
               (eval `{if (or (or (or (or false false) (or false false)) (or (or false false) (or false false))) (or (or (or false false) (or false false)) (or (or false false) (or false false)))) true false}) (v-bool #f)
  )
  
   ; - Zachary Robinson

  (test-equal? "Test AND short-circuit evaluation"
               (eval `{and false "dog"}) (v-bool #f)
  )
  (test-equal? "Test OR short-circuit evaluation"
               (eval `{or true "dog"}) (v-bool #t)
  )

  ;;Var Lam and App tests
   ; - Zachary Robinson
  (test-equal? "Test for e-app and e-lam from brown specs"
               (eval `((lam x (+ x 3)) 2))(v-num 5)
  )
  (test-equal? "Test for e-app and e-lam from brown specs"
               (eval `((lam y 5)1)) (v-num 5)
  )

  ;David Holstedt
  (test-equal? "Can create a function via lam and call it as first argument"
               (eval `( (lam x (+ x 1) ) 10) ) (v-num 11)
  )

  (test-equal? "Can use a function created by lambda as an argument in another lambda function"
               (eval `(
                       (lam f (f (f (f 2))))
                       (lam x (+ x 4))
                       )
                     )
               (v-num 14)
  )
  
  (test-equal? "meaning of variables changes depending on scope"
               (eval `(
                       (lam x ( (lam x (+ x 3)) x))
                       2))
               (v-num 5)
  )
  
  (test-equal? "Ackermannesqe lambda hell"
               (eval `(
                       ((lam y ((lam x (x x)) (y y)))
                        (lam x x))
                       56))
               (v-num 56)
  )
  
  (test-raises-interp-error? "Using a non-function as a function throws an error"
                             (eval `{"xerophytic" 5})
                             (err-not-a-function (v-str "xerophytic"))
  )

  (test-raises-interp-error? "Trying to interpret a never-defined variable throws an error"
                             (eval `{+ a 8000})
                             (err-unbound-var 'a)
  )

  (test-raises-interp-error? "Trying to interpret an out-of scope variable throws an error"
                             (eval `{ (lam x (+ x 5)) x})
                             (err-unbound-var 'x)
  )

  ;;Let tests
   ; - Rommel Ravanera 


  (test-equal? "Test LET using string"
               (eval `{let (x "this") x}) (v-str "this"))
  
  (test-equal? "Test LET using strings/append"
               (eval `{let (x "te") (++ x "st")}) (v-str "test"))

  (test-equal? "Test LET using strings/append - nested"
               (eval `{let (a "some") (let (b "thing") (++ a b))}) (v-str "something"))

  (test-equal? "Test LET using strings/append - nested part 2"
               (eval `{let (a "another") (let (b " ") (let (c "test") (++ a (++ b c))))}) (v-str "another test"))

  (test-equal? "Test LET using strings/append - deeply nested (idea borrowed and adapted from Evan's post as the credit goes to him, thanks Evan)"
               (eval `{let (a "how") (let (b " far ") (let (c "can ") (let (d "we") (let (e " go") (let (f "?") (++ a (++ b (++ c (++ d (++ e f))))))))))}) (v-str "how far can we go?"))

  (test-equal? "Test LET using bool"
               (eval `{let (x true) x}) (v-bool #t))

  (test-equal? "Test LET with AND using boolean"
               (eval `{let (x true) (and (num= (+ 2 0) (+ 1 1)) x)}) (v-bool #t))

  (test-equal? "Test LET with AND using boolean part 2"
               (eval `{let (x true) (let (y false) (and (and (num= (+ 1 18) (+ 9 10)) x) y))}) (v-bool #f))

  (test-equal? "Test LET with AND using addition and boolean"
               (eval `{let (x true) (and (num= (+ 2 1) (+ 0 3)) x)}) (v-bool #t))

  (test-equal? "Test LET with OR using boolean"
               (eval `{let (x true) (let (y false)(or x y))}) (v-bool #t))

  (test-equal? "Test LET with OR using boolean part 2"
               (eval `{let (x false) (let (y false) (or x y))}) (v-bool #f))

  (test-equal? "Test LET with AND/OR using addition and boolean"
               (eval `{let (x true) (let (y false) (and x (or (num= (+ 5 5) (+ 7 3)) y)))}) (v-bool #t))

  (test-equal? "Test LET with AND/OR using addition and boolean part 2"
               (eval `{let (x true) (let (y false) (or (and (num= (+ 67 33) (+ 25 75)) x) y))}) (v-bool #t))

  ; Zachary Robinson and Nicholas Keng

  (test-equal? "Test LET with sugar in variable assignment"
               (eval `{let (x (and true false)) x}) (v-bool #f)
  )

  (test-equal? "Test LET with sugar in the body"
               (eval `{let (x true) (and x x)}) (v-bool #t)
  )

  (test-equal? "Test LET with sugar in variable assignment and the body"
               (eval `{let (x (and true false)) (and x x)}) (v-bool #f)
  )
 )
;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))
