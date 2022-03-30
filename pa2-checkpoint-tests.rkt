#lang racket
;; Adapted by S. Rivoire from https://cs.brown.edu/courses/cs173/2021/implementation-specs/interp/interpreter-tests.rkt
;; my tests for SSU CS 460 Programming Assignment #2 checkpoint

;; =============================================================================
;; PA 2 Checkpoint Grading Scheme
;; 8 points: PA 1 functionality (EMRN scale)
;; 8 points: sugar-and, sugar-or (EMRN scale)
;; 3 points: interp function that takes 2 parameters. Graded by inspection.
;; 6 points: helper functions for hash env
;;    2 points: empty env; add a pair (graded by adding a pair to an empty env)
;;    4 points: lookup (2 pts for case where value is present, 2 pts for not found)
;; =============================================================================

;; =============================================================================
;; Interpreter: interpreter-tests.rkt
;; =============================================================================

(require (only-in "interpreter.rkt" eval)
         "support.rkt"
         "test-support.rkt")

;; DO NOT EDIT ABOVE THIS LINE =================================================
(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE =========

;; =============================================================================
;; PA2 TESTS start with II-
;; =============================================================================
  ;; Test II-A: Sugar-and with primitive boolean operands + simple error cases
  (test-equal? "II-A-1: AND of primitives false & false" (eval `(and false false)) (v-bool #f))
  (test-equal? "II-A-2: AND of primitives false & true" (eval `(and false true)) (v-bool #f))
  (test-equal? "II-A-3: AND of primitives true & false" (eval `(and true false)) (v-bool #f))
  (test-equal? "II-A-4: AND of primitives true & true" (eval `(and true true)) (v-bool #t))
  ;; Short-circuit evaluation: if first operand is false, don't evaluate the second
  ;; therefore, this expression shouldn't raise an error
  (test-equal? "II-A-5: Short-circuit AND of primitive false & a number" (eval `(and false 1)) (v-bool #f))
  ;; And now, actual error checking on ANDs w/ primitive operands
  (test-raises-interp-error? "II-A-6: AND(num, num) raises error"
                             (eval `(and 7 8))
                             (err-if-got-non-boolean (v-num 7)))
  (test-raises-interp-error? "II-A-7: AND(true, num) raises error"
                             (eval `(and true -2))
                             (err-if-got-non-boolean (v-num -2)))
  (test-raises-interp-error? "II-A-8: AND(num, true) raises error"
                             (eval `(and 37 true))
                             (err-if-got-non-boolean (v-num 37)))
  (test-raises-interp-error? "II-A-9: AND(num, str) raises error"
                             (eval `(and 7 "Ditto")) ; we're going with Pokémon
                             (err-if-got-non-boolean (v-num 7)))
  (test-raises-interp-error? "II-A-10: AND(str, num) raises error"
                             (eval `(and "Victreebel" 12))
                             (err-if-got-non-boolean (v-str "Victreebel")))
  (test-raises-interp-error? "II-A-11: AND(true, str) raises error"
                             (eval `(and true "Rapidash"))
                             (err-if-got-non-boolean (v-str "Rapidash")))
  (test-raises-interp-error? "II-A-12: AND(str, true) raises error"
                             (eval `(and "Golem" true))
                             (err-if-got-non-boolean (v-str "Golem")))
  
  ;; Test II-B: Sugar-or with primitive boolean operands
  (test-equal? "II-B-1: OR of primitives false & false" (eval `(or false false)) (v-bool #f))
  (test-equal? "II-B-2: OR of primitives false & true" (eval `(or false true)) (v-bool #t))
  (test-equal? "II-B-3: OR of primitives true & false" (eval `(or true false)) (v-bool #t))
  (test-equal? "II-B-4: OR of primitives true & true" (eval `(or true true)) (v-bool #t))
  ;; Short-circuit evaluation: if first operand is true, don't evaluate the second
  ;; therefore, this expression shouldn't raise an error
  (test-equal? "II-B-5: Short-circuit OR of primitive true & a number" (eval `(or true 1)) (v-bool #t))
  
  ;; And now, actual error checking on ORs w/ primitive operands
  (test-raises-interp-error? "II-B-6: OR(num, num) raises error"
                             (eval `(or 7 8))
                             (err-if-got-non-boolean (v-num 7)))
  (test-raises-interp-error? "II-B-7: OR(false, num) raises error"
                             (eval `(or false -2))
                             (err-if-got-non-boolean (v-num -2)))
  (test-raises-interp-error? "II-B-8: OR(num, false) raises error"
                             (eval `(or 37 false))
                             (err-if-got-non-boolean (v-num 37)))
  (test-raises-interp-error? "II-B-9: OR(num, str) raises error"
                             (eval `(or 7 "Poliwrath")) ; still on the Pokémon theme
                             (err-if-got-non-boolean (v-num 7)))
  (test-raises-interp-error? "II-B-10: OR(str, num) raises error"
                             (eval `(or "Jynx" 12))
                             (err-if-got-non-boolean (v-str "Jynx")))
  (test-raises-interp-error? "II-B-11: OR(false, str) raises error"
                             (eval `(or false "Chansey"))
                             (err-if-got-non-boolean (v-str "Chansey")))
  (test-raises-interp-error? "II-B-12: OR(str, false) raises error"
                             (eval `(or "Seaking" true))
                             (err-if-got-non-boolean (v-str "Seaking")))

  ;; Test II-C: Sugar-and with recursive operands
  (test-equal? "II-C-1: AND of false num= expression & primitive true ==> false" ; credit to Jessica Wood and Ethan Zogg
             (eval `(and (num= 1 2) true)) (v-bool #f))
  (test-equal? "II-C-2: AND of true num= expression & primitive true ==> true"
             (eval `(and (num= 65 65) true)) (v-bool #t))
  (test-equal? "II-C-3: AND of primitive true & false num= expr ==> false"
             (eval `(and true (num= 1 2))) (v-bool #f))
  (test-equal? "II-C-4: AND of primitive true & true num= expr ==> true"
             (eval `(and true (num= -99 -99))) (v-bool #t))
  (test-equal? "II-C-5: AND short-circuit evaluation: false num= expression & str ==> false"
             (eval `(and (num= 2 1) "howdy")) (v-bool #f))
  (test-equal? "II-C-6: AND of false str= expression & primitive true ==> false"
             (eval `(and (str= "hi" "sup") true)) (v-bool #f))
  (test-equal? "II-C-7: AND of true str= expression & primitive true ==> true"  ; credit to Jessica Wood and Ethan Zogg
             (eval `(and (str= "hi" "hi") true)) (v-bool #t))
  (test-equal? "II-C-8: AND of primitive true & false str= expr ==> false"
             (eval `(and true (str= "hi" "sup"))) (v-bool #f))
  (test-equal? "II-C-9: AND of primitive true & true str= expr ==> true"
             (eval `(and true (str= "hey" "hey"))) (v-bool #t))
  (test-equal? "II-C-10: AND short-circuit evaluation: false str= expression & str ==> false"
             (eval `(and (str= "hey" "howdy") "hi")) (v-bool #f))
  (test-equal? "II-C-11: AND of two false num= expressions ==> false"
             (eval `(and (num= 1 2) (num= 0 1))) (v-bool #f))
  (test-equal? "II-C-12: AND of a false num= and a true num= ==> false"
             (eval `(and (num= 3 5) (num= 65 65))) (v-bool #f))
  (test-equal? "II-C-13: AND of a true num= and a false num= ==> false"
             (eval `(and (num= -80 -80) (num= 1 65))) (v-bool #f))
  (test-equal? "II-C-14: AND of a true num= and a true num= ==> true"   ; credit to Jessica Wood and Ethan Zogg
             (eval `(and (num= 1 1) (num= 100 100))) (v-bool #t))
  (test-equal? "II-C-15: AND of two false str= expressions ==> false"
             (eval `(and (str= "hey" "howdy") (str= "one" "1"))) (v-bool #f))
  (test-equal? "II-C-16: AND of a false str= and a true str= ==> false"  ; credit to Christopher Murray
             (eval `(and (str= "one" "1") (str= "hello" "hello"))) (v-bool #f))
  (test-equal? "II-C-17: AND of a true str= and a false str= ==> false"
             (eval `(and (str= "hello" "hello") (str= "1" "65"))) (v-bool #f))
  (test-equal? "II-C-18: AND of a true str= and a true str= ==> true"
             (eval `(and (str= "1" "1") (str= "h3110" "h3110"))) (v-bool #t))

  ;; AND inside if-expressions
  (test-equal? "II-C-19: AND(false, false) as if-condition"
             (eval `(if (and false false) -4 -37)) (v-num -37))
  (test-equal? "II-C-20: AND(false, true) as if-condition"
             (eval `(if (and false true) 98 84)) (v-num 84))
  (test-equal? "II-C-21: AND(true, false) as if-condition"
             (eval `(if (and true false) -7 6)) (v-num 6))
  (test-equal? "II-C-22: AND(true, true) as if-condition"
             (eval `(if (and true true) -19 32)) (v-num -19))
  (test-equal? "II-C-23: AND(false, str) as if-condition: should short-circuit w/o error"
             (eval `(if (and false "meow") 7 87)) (v-num 87))
  
  ;; Nested ANDs in if-conditions
  (test-equal? "II-C-24a: AND(false, AND(true, false)) as if-condition"
             (eval `(if (and false (and true false)) -4 -37)) (v-num -37))
  (test-equal? "II-C-24b: AND(AND(true, false), false) as if-condition"
             (eval `(if (and (and true false) false) -4 -37)) (v-num -37))
  (test-equal? "II-C-24c: AND(AND(true, false), AND(true, false)) as if-condition"
             (eval `(if (and (and true false) (and true false)) -4 -37)) (v-num -37))
  (test-equal? "II-C-25a: AND(false, AND(true, true)) as if-condition"
             (eval `(if (and false (and true true)) -4 -37)) (v-num -37))
  (test-equal? "II-C-25b: AND(AND(true, true), false) as if-condition"
             (eval `(if (and (and true true) false) -4 -37)) (v-num -37))
  (test-equal? "II-C-26a: AND(true, AND(true, false)) as if-condition"
             (eval `(if (and true (and true false)) -4 -37)) (v-num -37))
  (test-equal? "II-C-26b: AND(AND(true, false), true) as if-condition"
             (eval `(if (and (and true false) false) -4 -37)) (v-num -37))
  (test-equal? "II-C-27a: AND(true, AND(true, true)) as if-condition"
             (eval `(if (and true (and true true)) -4 -37)) (v-num -4))
  (test-equal? "II-C-27b: AND(AND(true, true), true) as if-condition"
             (eval `(if (and (and true true) true) -4 -37)) (v-num -4))
  (test-equal? "II-C-27c: AND(AND(true, true), AND(true, true)) as if-condition"
             (eval `(if (and (and true true) (and true true)) -4 -37)) (v-num -4))

  ;; AND as part of e-op expression
  (test-equal? "II-C-28a: + with AND(false, true) deep inside left subtree"
             (eval `(+ (+ (+ (+ (+ (if (and false true) 1 0) 1) 1) 1) 1) 1)) (v-num 5))
  (test-equal? "II-C-28b: + with AND(true, true) deep inside left subtree"
             (eval `(+ (+ (+ (+ (+ (if (and true true) 1 0) 1) 1) 1) 1) 1)) (v-num 6))
  (test-equal? "II-C-29a: + with AND(false, true) deep inside right subtree"
             (eval `(+ 1 (+ 1 (+ 1 (+ 1 (if (and false true) 1 0)))))) (v-num 4))
  (test-equal? "II-C-29b: + with AND(true, true) deep inside right subtree"
             (eval `(+ 1 (+ 1 (+ 1 (+ 1 (if (and true true) 1 0)))))) (v-num 5))
  (test-equal? "II-C-30a: ++ with AND(false, true) deep inside left subtree"
             (eval `(++ (++ (++ (++ (++ (if (and false true) "a" "b") "c") "d") "e") "f") "g")) (v-str "bcdefg"))
  (test-equal? "II-C-30b: ++ with AND(true, true) deep inside left subtree"
             (eval `(++ (++ (++ (++ (++ (if (and true true) "a" "b") "c") "d") "e") "f") "g")) (v-str "acdefg"))
  (test-equal? "II-C-31a: ++ with AND(false, true) deep inside right subtree"
             (eval `(++ "a" (++ "b" (++ "c" (++ "d" (if (and false true) "e" "f")))))) (v-str "abcdf"))
  (test-equal? "II-C-31b: ++ with AND(true, true) deep inside right subtree"
             (eval `(++ "a" (++ "b" (++ "c" (++ "d" (if (and true true) "e" "f")))))) (v-str "abcde"))
  (test-equal? "II-C-32a: num= with AND(false, true) inside left subtree"
             (eval `(num= (if (and false true) 1 2) 2)) (v-bool #t))
  (test-equal? "II-C-32b: num= with AND(true, true) inside left subtree"
             (eval `(num= (if (and true true) 1 2) 2)) (v-bool #f))
  (test-equal? "II-C-32c: num= with AND(false, true) inside right subtree"
             (eval `(num= 2 (if (and false true) 1 2))) (v-bool #t))
  (test-equal? "II-C-32d: num= with AND(true, true) inside right subtree"
             (eval `(num= 2 (if (and true true) 1 2))) (v-bool #f))
  (test-equal? "II-C-33a: str= with AND(false, true) inside left subtree"
             (eval `(str= (if (and false true) "a" "b") "b")) (v-bool #t))
  (test-equal? "II-C-33b: str= with AND(true, true) inside left subtree"
             (eval `(str= (if (and true true) "a" "b") "b")) (v-bool #f))
  (test-equal? "II-C-33c: str= with AND(false, true) inside right subtree"
             (eval `(str= "b" (if (and false true) "a" "b"))) (v-bool #t))
  (test-equal? "II-C-33d: str= with AND(true, true) inside right subtree"
             (eval `(str= "b" (if (and true true) "a" "b"))) (v-bool #f))

  ;; Student contributions
  (test-equal? "II-C-34: From Piazza: AND using recursive str= and num=" ; C. Murray
               (eval `(and (str= (++ "rac" "car") "racecar") (num= (+ 23 54) 56)))
               (v-bool #f))
  (test-equal? "II-C-35: From Piazza: AND using recursion"  ; C. Murray
               (eval `(and (str= (++ "race" "car") "racecar") (and (num= 1 1) (str= "eq" "eq"))))
               (v-bool #t))
  (test-equal? "II-C-36: From Piazza: AND using recursive str= and num="  ; C. Murray
               (eval `(and (str= (++ "rac" "car") "racecar") (num= (+ 23 54) 56)))
               (v-bool #f))
  (test-equal? "II-C-37: From Piazza: AND using recursion"  ; C. Murray
               (eval `(and (str= (++ "race" "car") "racecar") (and (num= 1 1) (str= "eq" "eq"))))
               (v-bool #t))
  (test-equal? "II-C-38: From Piazza: AND with deeply nested left operand"   ; J. Wood & E. Zogg
              (eval `(and (and (and (num= 1 1) (str= "p" "p")) true) false)) (v-bool false))

  ;; Test II-D: Sugar-or with recursive operands
  (test-equal? "II-D-1: OR of false num= expression & primitive false ==> false"
             (eval `(or (num= 1 2) false)) (v-bool #f))
  (test-equal? "II-D-2: OR of true num= expression & primitive false ==> true"  ; credit to Jessica Wood and Ethan Zogg
             (eval `(or (num= 1 1) false)) (v-bool #t))
  (test-equal? "II-D-3: OR of primitive false & false num= expr ==> false"
             (eval `(or false (num= 1 2))) (v-bool #f))
  (test-equal? "II-D-4: OR of primitive false & true num= expr ==> true"
             (eval `(or false (num= -99 -99))) (v-bool #t))
  (test-equal? "II-D-5: OR short-circuit evaluation: true num= expression & str ==> true"
             (eval `(or (num= 2 2) "howdy")) (v-bool #t))
  (test-equal? "II-D-6: OR of false str= expression & primitive false ==> false"
             (eval `(or (str= "hi" "sup") false)) (v-bool #f))
  (test-equal? "II-D-7: OR of true str= expression & primitive false ==> true"
             (eval `(or (str= "hi" "hi") false)) (v-bool #t))
  (test-equal? "II-D-8: OR of primitive false & false str= expr ==> false"
             (eval `(or false (str= "hi" "sup"))) (v-bool #f))
  (test-equal? "II-D-9: OR of primitive false & true str= expr ==> true"
             (eval `(or false (str= "hey" "hey"))) (v-bool #t))
  (test-equal? "II-D-10: OR short-circuit evaluation: true str= expression & str ==> true"
             (eval `(or (str= "hey" "hey") "hi")) (v-bool #t))
  (test-equal? "II-D-11: OR of two false num= expressions ==> false"
             (eval `(or (num= 1 2) (num= 0 1))) (v-bool #f))
  (test-equal? "II-D-12: OR of a false num= and a true num= ==> true"
             (eval `(or (num= 3 5) (num= 65 65))) (v-bool #t))
  (test-equal? "II-D-13: OR of a true num= and a false num= ==> true"
             (eval `(or (num= -80 -80) (num= 1 65))) (v-bool #t))
  (test-equal? "II-D-14: OR of a true num= and a true num= ==> true"
             (eval `(or (num= 1 1) (num= 100 100))) (v-bool #t))
  (test-equal? "II-D-15: OR of two false str= expressions ==> false"
             (eval `(or (str= "hey" "howdy") (str= "one" "1"))) (v-bool #f))
  (test-equal? "II-D-16: OR of a false str= and a true str= ==> true"
             (eval `(or (str= "one" "1") (str= "hello" "hello"))) (v-bool #t))
  (test-equal? "II-D-17: OR of a true str= and a false str= ==> true"
             (eval `(or (str= "hello" "hello") (str= "1" "65"))) (v-bool #t))
  (test-equal? "II-D-18: OR of a true str= and a true str= ==> true"
             (eval `(or (str= "1" "1") (str= "h3110" "h3110"))) (v-bool #t))
 
  ;; OR inside if-expressions
  (test-equal? "II-D-19: OR(false, false) as if-condition"
             (eval `(if (or false false) -4 -37)) (v-num -37))
  (test-equal? "II-D-20: OR(false, true) as if-condition"
             (eval `(if (or false true) 98 84)) (v-num 98))
  (test-equal? "II-D-21: OR(true, false) as if-condition"
             (eval `(if (or true false) -7 6)) (v-num -7))
  (test-equal? "II-D-22: OR(true, true) as if-condition"
             (eval `(if (or true true) -19 32)) (v-num -19))
  (test-equal? "II-D-23: OR(true, str) as if-condition: should short-circuit w/o error"
             (eval `(if (or true "meow") 7 87)) (v-num 7))
  
  ;; Nested OR in if-conditions
  (test-equal? "II-D-24a: OR(false, OR(false, true)) as if-condition"
             (eval `(if (or false (or false true)) -4 -37)) (v-num -4))
  (test-equal? "II-D-24b: OR(OR(false, true), false) as if-condition"
             (eval `(if (or (or false true) false) -4 -37)) (v-num -4))
  (test-equal? "II-D-24c: OR(OR(false, false), OR(false, true)) as if-condition"
             (eval `(if (or (or false false) (or false true)) -4 -37)) (v-num -4))
  (test-equal? "II-D-25a: OR(true, OR(true, true)) as if-condition"
             (eval `(if (or true (or true true)) -4 -37)) (v-num -4))
  (test-equal? "II-D-25b: OR(OR(true, true), true) as if-condition"
             (eval `(if (or (or true true) true) -4 -37)) (v-num -4))
  (test-equal? "II-D-26a: OR(false, OR(false, false)) as if-condition"
             (eval `(if (or false (or false false)) -4 -37)) (v-num -37))
  (test-equal? "II-D-26b: OR(OR(false, false), false) as if-condition"
             (eval `(if (or (or false false) false) -4 -37)) (v-num -37))
  (test-equal? "II-D-26c: OR(OR(false, false), OR(false, false)) as if-condition"
             (eval `(if (or (or false false) (or false false)) -4 -37)) (v-num -37))
  (test-equal? "II-D-27a: OR(or(true, false), false) as if-condition"
             (eval `(if (or (or true false) false) -4 -37)) (v-num -4))
  (test-equal? "II-D-27b: OR(OR(true, false), OR(false, false)) as if-condition"
             (eval `(if (or (or true false) (or false false)) -4 -37)) (v-num -4))
    
  ;; OR as part of e-op expression
  (test-equal? "II-D-28a: + with OR(false, false) deep inside left subtree"
             (eval `(+ (+ (+ (+ (+ (if (or false false) 1 0) 1) 1) 1) 1) 1)) (v-num 5))
  (test-equal? "II-D-28b: + with OR(false, true) deep inside left subtree"
             (eval `(+ (+ (+ (+ (+ (if (or false true) 1 0) 1) 1) 1) 1) 1)) (v-num 6))
  (test-equal? "II-D-29a: + with OR(false, false) deep inside right subtree"
             (eval `(+ 1 (+ 1 (+ 1 (+ 1 (if (or false false) 1 0)))))) (v-num 4))
  (test-equal? "II-D-29b: + with OR(false, true) deep inside right subtree"
             (eval `(+ 1 (+ 1 (+ 1 (+ 1 (if (or false true) 1 0)))))) (v-num 5))
  (test-equal? "II-D-30a: ++ with OR(false, false) deep inside left subtree"
             (eval `(++ (++ (++ (++ (++ (if (or false false) "a" "b") "c") "d") "e") "f") "g")) (v-str "bcdefg"))
  (test-equal? "II-D-30b: ++ with OR(false, true) deep inside left subtree"
             (eval `(++ (++ (++ (++ (++ (if (or false true) "a" "b") "c") "d") "e") "f") "g")) (v-str "acdefg"))
  (test-equal? "II-D-31a: ++ with OR(false, false) deep inside right subtree"
             (eval `(++ "a" (++ "b" (++ "c" (++ "d" (if (or false false) "e" "f")))))) (v-str "abcdf"))
  (test-equal? "II-D-31b: ++ with OR(false, true) deep inside right subtree"
             (eval `(++ "a" (++ "b" (++ "c" (++ "d" (if (or false true) "e" "f")))))) (v-str "abcde"))
  (test-equal? "II-D-32a: num= with OR(false, false) inside left subtree"
             (eval `(num= (if (or false false) 1 2) 2)) (v-bool #t))
  (test-equal? "II-D-32b: num= with OR(false, true) inside left subtree"
             (eval `(num= (if (or false true) 1 2) 2)) (v-bool #f))
  (test-equal? "II-D-32c: num= with OR(false, false) inside right subtree"
             (eval `(num= 2 (if (or false false) 1 2))) (v-bool #t))
  (test-equal? "II-D-32d: num= with OR(false, true) inside right subtree"
             (eval `(num= 2 (if (or false true) 1 2))) (v-bool #f))
  (test-equal? "II-D-33a: str= with OR(false, false) inside left subtree"
             (eval `(str= (if (or false false) "a" "b") "b")) (v-bool #t))
  (test-equal? "II-D-33b: str= with OR(false, true) inside left subtree"
             (eval `(str= (if (or false true) "a" "b") "b")) (v-bool #f))
  (test-equal? "II-D-33c: str= with OR(false, false) inside right subtree"
             (eval `(str= "b" (if (or false false) "a" "b"))) (v-bool #t))
  (test-equal? "II-D-33d: str= with OR(false, true) inside right subtree"
             (eval `(str= "b" (if (or false true) "a" "b"))) (v-bool #f))

  ;; Student contributions
  (test-equal? "II-D-34: From Piazza: OR using num= and str=" ; credit to C. Murray
               (eval `(or (num= (+ 4 1) (+ 3 2))(str= (++ "cor" "rect") (++ "co" (++ "rre" "ct")))))
               (v-bool #t))
  (test-equal? "II-D-35: From Piazza: OR using num= and str=" ; credit to C. Murray
               (eval `(or (num= (+ 12 6) (+ 9 7))(str= (++ "inc" "orrect") (++ "in" (++ "cor" "rect")))))
               (v-bool #t))
  (test-equal? "II-D-36: From Piazza: OR using recursion" ; credit to C. Murray
               (eval `(or (num= 46 (+ 40 7)) (or (str= "is it" "is it not") (num= 46897 (+ 4671 42226)))))
               (v-bool #t))
  (test-equal? "II-D-37: From Piazza: OR with deeply nested left operand"   ; credit to J. Wood and E. Zogg
              (eval `(or (or (or (num= 1 1) (str= "p" "p")) true) false)) (v-bool true))

;; =============================================================================
;; PA1 TESTS
;; =============================================================================
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
  ; 7b: Credit: David Holstedt
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
)
;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))