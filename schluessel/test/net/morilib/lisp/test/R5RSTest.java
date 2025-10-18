/*
 * Copyright 2009 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lisp.test;

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Undef;

public class R5RSTest extends TCSubr {
	
	public void testPredict1() {
		Scheme l = Scheme.newInstance();
		
		eq   (l,"(boolean?   #f)", T);
		eq   (l,"(pair?      #f)", F);
		eq   (l,"(symbol?    #f)", F);
		eq   (l,"(number?    #f)", F);
		eq   (l,"(char?      #f)", F);
		eq   (l,"(string?    #f)", F);
		eq   (l,"(vector?    #f)", F);
		eq   (l,"(port?      #f)", F);
		eq   (l,"(procedure? #f)", F);
		
		eq   (l,"(boolean?   '(1))", F);
		eq   (l,"(pair?      '(1))", T);
		eq   (l,"(symbol?    '(1))", F);
		eq   (l,"(number?    '(1))", F);
		eq   (l,"(char?      '(1))", F);
		eq   (l,"(string?    '(1))", F);
		eq   (l,"(vector?    '(1))", F);
		eq   (l,"(port?      '(1))", F);
		eq   (l,"(procedure? '(1))", F);
		
		eq   (l,"(boolean?   'a)", F);
		eq   (l,"(pair?      'a)", F);
		eq   (l,"(symbol?    'a)", T);
		eq   (l,"(number?    'a)", F);
		eq   (l,"(char?      'a)", F);
		eq   (l,"(string?    'a)", F);
		eq   (l,"(vector?    'a)", F);
		eq   (l,"(port?      'a)", F);
		eq   (l,"(procedure? 'a)", F);
		
		eq   (l,"(boolean?   1)", F);
		eq   (l,"(pair?      1)", F);
		eq   (l,"(symbol?    1)", F);
		eq   (l,"(number?    1)", T);
		eq   (l,"(char?      1)", F);
		eq   (l,"(string?    1)", F);
		eq   (l,"(vector?    1)", F);
		eq   (l,"(port?      1)", F);
		eq   (l,"(procedure? 1)", F);
		
		eq   (l,"(boolean?   100000000000000000000)", F);
		eq   (l,"(pair?      100000000000000000000)", F);
		eq   (l,"(symbol?    100000000000000000000)", F);
		eq   (l,"(number?    100000000000000000000)", T);
		eq   (l,"(char?      100000000000000000000)", F);
		eq   (l,"(string?    100000000000000000000)", F);
		eq   (l,"(vector?    100000000000000000000)", F);
		eq   (l,"(port?      100000000000000000000)", F);
		eq   (l,"(procedure? 100000000000000000000)", F);
		
		eq   (l,"(boolean?   1/2)", F);
		eq   (l,"(pair?      1/2)", F);
		eq   (l,"(symbol?    1/2)", F);
		eq   (l,"(number?    1/2)", T);
		eq   (l,"(char?      1/2)", F);
		eq   (l,"(string?    1/2)", F);
		eq   (l,"(vector?    1/2)", F);
		eq   (l,"(port?      1/2)", F);
		eq   (l,"(procedure? 1/2)", F);
		
		eq   (l,"(boolean?   1.2)", F);
		eq   (l,"(pair?      1.2)", F);
		eq   (l,"(symbol?    1.2)", F);
		eq   (l,"(number?    1.2)", T);
		eq   (l,"(char?      1.2)", F);
		eq   (l,"(string?    1.2)", F);
		eq   (l,"(vector?    1.2)", F);
		eq   (l,"(port?      1.2)", F);
		eq   (l,"(procedure? 1.2)", F);
		
		eq   (l,"(boolean?   1+i)", F);
		eq   (l,"(pair?      1+i)", F);
		eq   (l,"(symbol?    1+i)", F);
		eq   (l,"(number?    1+i)", T);
		eq   (l,"(char?      1+i)", F);
		eq   (l,"(string?    1+i)", F);
		eq   (l,"(vector?    1+i)", F);
		eq   (l,"(port?      1+i)", F);
		eq   (l,"(procedure? 1+i)", F);
		
		eq   (l,"(boolean?   #\\a)", F);
		eq   (l,"(pair?      #\\a)", F);
		eq   (l,"(symbol?    #\\a)", F);
		eq   (l,"(number?    #\\a)", F);
		eq   (l,"(char?      #\\a)", T);
		eq   (l,"(string?    #\\a)", F);
		eq   (l,"(vector?    #\\a)", F);
		eq   (l,"(port?      #\\a)", F);
		eq   (l,"(procedure? #\\a)", F);
		
		eq   (l,"(boolean?   \"a\")", F);
		eq   (l,"(pair?      \"a\")", F);
		eq   (l,"(symbol?    \"a\")", F);
		eq   (l,"(number?    \"a\")", F);
		eq   (l,"(char?      \"a\")", F);
		eq   (l,"(string?    \"a\")", T);
		eq   (l,"(vector?    \"a\")", F);
		eq   (l,"(port?      \"a\")", F);
		eq   (l,"(procedure? \"a\")", F);
		
		eq   (l,"(boolean?   '#(1))", F);
		eq   (l,"(pair?      '#(1))", F);
		eq   (l,"(symbol?    '#(1))", F);
		eq   (l,"(number?    '#(1))", F);
		eq   (l,"(char?      '#(1))", F);
		eq   (l,"(string?    '#(1))", F);
		eq   (l,"(vector?    '#(1))", T);
		eq   (l,"(port?      '#(1))", F);
		eq   (l,"(procedure? '#(1))", F);
		
		eq   (l,"(boolean?   (current-input-port))", F);
		eq   (l,"(pair?      (current-input-port))", F);
		eq   (l,"(symbol?    (current-input-port))", F);
		eq   (l,"(number?    (current-input-port))", F);
		eq   (l,"(char?      (current-input-port))", F);
		eq   (l,"(string?    (current-input-port))", F);
		eq   (l,"(vector?    (current-input-port))", F);
		eq   (l,"(port?      (current-input-port))", T);
		eq   (l,"(procedure? (current-input-port))", F);
		
		eq   (l,"(boolean?   (current-output-port))", F);
		eq   (l,"(pair?      (current-output-port))", F);
		eq   (l,"(symbol?    (current-output-port))", F);
		eq   (l,"(number?    (current-output-port))", F);
		eq   (l,"(char?      (current-output-port))", F);
		eq   (l,"(string?    (current-output-port))", F);
		eq   (l,"(vector?    (current-output-port))", F);
		eq   (l,"(port?      (current-output-port))", T);
		eq   (l,"(procedure? (current-output-port))", F);
		
		eq   (l,"(boolean?   car)", F);
		eq   (l,"(pair?      car)", F);
		eq   (l,"(symbol?    car)", F);
		eq   (l,"(number?    car)", F);
		eq   (l,"(char?      car)", F);
		eq   (l,"(string?    car)", F);
		eq   (l,"(vector?    car)", F);
		eq   (l,"(port?      car)", F);
		eq   (l,"(procedure? car)", T);
		
		eq   (l,"(boolean?   (lambda (x) x))", F);
		eq   (l,"(pair?      (lambda (x) x))", F);
		eq   (l,"(symbol?    (lambda (x) x))", F);
		eq   (l,"(number?    (lambda (x) x))", F);
		eq   (l,"(char?      (lambda (x) x))", F);
		eq   (l,"(string?    (lambda (x) x))", F);
		eq   (l,"(vector?    (lambda (x) x))", F);
		eq   (l,"(port?      (lambda (x) x))", F);
		eq   (l,"(procedure? (lambda (x) x))", T);
		
		l.exec ("(define cont #f)");
		l.exec ("(call/cc (lambda (k) (set! cont k)))");
		eq   (l,"(boolean?   cont)", F);
		eq   (l,"(pair?      cont)", F);
		eq   (l,"(symbol?    cont)", F);
		eq   (l,"(number?    cont)", F);
		eq   (l,"(char?      cont)", F);
		eq   (l,"(string?    cont)", F);
		eq   (l,"(vector?    cont)", F);
		eq   (l,"(port?      cont)", F);
		eq   (l,"(procedure? cont)", T);
		
		eq   (l,"(boolean?   '())", F);
		eq   (l,"(pair?      '())", F);
		eq   (l,"(symbol?    '())", F);
		eq   (l,"(number?    '())", F);
		eq   (l,"(char?      '())", F);
		eq   (l,"(string?    '())", F);
		eq   (l,"(vector?    '())", F);
		eq   (l,"(port?      '())", F);
		eq   (l,"(procedure? '())", F);
		eq   (l,"(null?      '())", T);
	}
	
	public void testC4_1_2() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(quote a)", sym("a"));
		equal(l,"(quote #(a b c))", vec(sym("a"), sym("b"), sym("c")));
		equal(l,"(quote (+ 1 2))", list(sym("+"), 1, 2));
		equal(l,"'a", sym("a"));
		equal(l,"'#(a b c)", vec(sym("a"), sym("b"), sym("c")));
		equal(l,"'()", Nil.NIL);
		equal(l,"'(+ 1 2)", list(sym("+"), 1, 2));
		equal(l,"'(quote a)", list(sym("quote"), sym("a")));
		equal(l,"''a", list(sym("quote"), sym("a")));
		equal(l,"'\"abc\"", str("abc"));
		equal(l,"\"abc\"", str("abc"));
		equal(l,"'145932", newZ(145932));
		equal(l,"145932", newZ(145932));
		equal(l,"'#t", T);
		equal(l,"#t", T);
	}
	
	public void testC4_1_3() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(+ 3 4)", newZ(7));
		equal(l,"((if #f + *) 3 4)", newZ(12));
	}
	
	public void testC4_1_4() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(procedure? (lambda (x) (+ x x)))", T);
		equal(l,"((lambda (x) (+ x x)) 4)", newZ(8));
		l.exec ("(define reverse-subtract" +
				"  (lambda (x y) (- y x)))");
		equal(l,"(reverse-subtract 7 10)", newZ(3));
		l.exec ("(define add4" +
				"  (let ((x 4))" +
				"    (lambda (y) (+ x y))))");
		equal(l,"(add4 6)", newZ(10));
		equal(l,"((lambda x x) 3 4 5 6)", list(3, 4, 5, 6));
		equal(l,"((lambda (x y . z) z) 3 4 5 6)", list(5, 6));
	}
	
	public void testC4_1_5() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(if (> 3 2) 'yes 'no)", sym("yes"));
		equal(l,"(if (> 2 3) 'yes 'no)", sym("no"));
		equal(l,"(if (> 3 2) (- 3 2) (+ 3 2))", newZ(1));
	}
	
	public void testC4_1_6() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define x 2)");
		equal(l,"(+ x 1)", newZ(3));
		equal(l,"(set! x 4)", Undef.UNDEF);
		equal(l,"(+ x 1)", newZ(5));
	}
	
	public void testC4_2_1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(cond ((> 3 2) 'greater)" +
				"      ((< 3 2) 'less))", sym("greater"));
		equal(l,"(cond ((> 3 3) 'greater)" +
				"      ((< 3 3) 'less)" +
				"      (else 'equal))", sym("equal"));
		equal(l,"(cond ((assv 'b '((a 1) (b 2))) => cadr)" +
				"      (else #f))", newZ(2));
		
		equal(l,"(case (* 2 3)" +
				"  ((2 3 5 7) 'prime)" +
				"  ((1 4 6 8 9) 'composite))", sym("composite"));
		equal(l,"(case (car '(c d))" +
				"  ((a) 'a)" +
				"  ((b) 'b))", Undef.UNDEF);
		equal(l,"(case (car '(c d))" +
				"  ((a e i o u) 'vowel)" +
				"  ((w y) 'semivowel)" +
				"  (else 'consonant))", sym("consonant"));
		equal(l,"(and (= 2 2) (> 2 1))", T);
		equal(l,"(and (= 2 2) (< 2 1))", F);
		equal(l,"(and 1 2 'c '(f g))", list(sym("f"), sym("g")));
		equal(l,"(and)", T);
		equal(l,"(or (= 2 2) (> 2 1))", T);
		equal(l,"(or (= 2 2) (< 2 1))", T);
		equal(l,"(or #f #f #f)", F);
		equal(l,"(or (memq 'b '(a b c))" +
				"    (/ 3 0))", list(sym("b"), sym("c")));
	}
	
	public void testC4_2_2() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(let ((x 2) (y 3))" +
				"  (* x y))", newZ(6));
		equal(l,"(let ((x 2) (y 3))" +
				"  (let ((x 7)" +
				"        (z (+ x y)))" +
				"    (* z x)))", newZ(35));
		equal(l,"(let ((x 2) (y 3))" +
				"  (let* ((x 7)" +
				"        (z (+ x y)))" +
				"    (* z x)))", newZ(70));
		equal(l,"(letrec ((even?" +
				"         (lambda (n)" +
				"           (if (zero? n)" +
				"               #t" +
				"               (odd? (- n 1)))))" +
				"         (odd?" +
				"         (lambda (n)" +
				"           (if (zero? n)" +
				"           #f" +
				"           (even? (- n 1))))))" +
				"  (even? 88))", T);
	}
	
	public void testC4_2_3() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define x 0)");
		equal(l,"(begin (set! x 5)" +
				"       (+ x 1))", newZ(6));
		equal(l,"(begin (display \"\\\"4\\\" plus 1 equals \")" +
				"       (display (+ 4 1)))", Undef.UNDEF);
	}
	
	public void testC4_2_4() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(do ((vec (make-vector 5))" +
				"     (i 0 (+ i 1)))" +
				"    ((= i 5) vec)" +
				"  (vector-set! vec i i))", vec(0, 1, 2, 3, 4));
		equal(l,"(let ((x '(1 3 5 7 9)))" +
				"  (do ((x x (cdr x))" +
				"       (sum 0 (+ sum (car x))))" +
				"      ((null? x) sum)))", newZ(25));
		equal(l,"(let loop ((numbers '(3 -2 1 6 -5))" +
				"           (nonneg '())" +
				"           (neg '()))" +
				"  (cond ((null? numbers) (list nonneg neg))" +
				"        ((>= (car numbers) 0)" +
				"         (loop (cdr numbers)" +
				"               (cons (car numbers) nonneg)" +
				"               neg))" +
				"        ((< (car numbers) 0)" +
				"         (loop (cdr numbers)" +
				"               nonneg" +
				"               (cons (car numbers) neg)))))",
				list(list(6, 1, 3), list(-5, -2)));
	}
	
	public void testC4_2_6() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"`(list ,(+ 1 2) 4)", list(sym("list"), 3, 4));
		equal(l,"(let ((name 'a))" +
				"  `(list ,name ',name))",
				list(sym("list"), sym("a"), list(sym("quote"), sym("a"))));
		equal(l,"`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
				list(sym("a"), 3, 4, 5, 6, sym("b")));
		equal(l,"`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
				cons(list(sym("foo"), 7), sym("cons")));
		equal(l,"`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
				vec(10, 5, 2, 4, 3, 8));
		equal(l,"`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
				vec(10, 5, 2, 4, 3, 8));
		equal(l,"`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)",
				list(sym("a"), qq(list(sym("b"), uq(list(sym("+"), 1, 2)),
						uq(list(sym("foo"), 4, sym("d"))), sym("e"))),
						sym("f")));
		equal(l,"(let ((name1 'x)" +
				"      (name2 'y))" +
				"  `(a `(b ,,name1 ,',name2 d) e))",
				list(sym("a"), qq(list(sym("b"), uq(sym("x")),
						uq(qt(sym("y"))), sym("d"))), sym("e")));
		equal(l,"(quasiquote (list (unquote (+ 1 2)) 4))",
				list(sym("list"), 3, 4));
		equal(l,"'(quasiquote (list (unquote (+ 1 2)) 4))",
				qq(list(sym("list"), uq(list(sym("+"), 1, 2)), 4)));
	}
	
	public void testC4_3_1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(let-syntax ((when (syntax-rules ()" +
				"                     ((when test stmt1 stmt2 ...)" +
				"                      (if test" +
				"                          (begin stmt1" +
				"                                 stmt2 ...))))))" +
				"  (let ((if #t))" +
				"    (when if (set! if 'now))" +
				"    if))", sym("now"));
		equal(l,"(let ((x 'outer))" +
				"  (let-syntax ((m (syntax-rules () ((m) x))))" +
				"    (let ((x 'inner))" +
				"      (m))))", sym("outer"));
		equal(l,"(letrec-syntax" +
				"  ((my-or (syntax-rules ()" +
				"            ((my-or) #f)" +
				"            ((my-or e) e)" +
				"            ((my-or e1 e2 ...)" +
				"             (let ((temp e1))" +
				"               (if temp" +
				"                   temp" +
				"                   (my-or e2 ...)))))))" +
				"  (let ((x #f)" +
				"        (y 7)" +
				"        (temp 8)" +
				"        (let odd?)" +
				"        (if even?))" +
				"    (my-or x" +
				"           (let temp)" +
				"           (if y)" +
				"           y)))", newZ(7));
	}
	
	public void testC5_2_1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define add3" +
				"  (lambda (x) (+ x 3)))");
		equal(l,"(add3 3)", newZ(6));
		l.exec ("(define first car)");
		equal(l,"(first '(1 2))", newZ(1));
	}
	
	public void testC5_2_2() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(let ((x 5))" +
				"  (define foo (lambda (y) (bar x y)))" +
				"  (define bar (lambda (a b) (+ (* a b) a)))" +
				"  (foo (+ x 3)))", newZ(45));
		equal(l,"(let ((x 5))" +
				"  (letrec ((foo (lambda (y) (bar x y)))" +
				"    (bar (lambda (a b) (+ (* a b) a))))" +
				"      (foo (+ x 3))))", newZ(45));
	}
	
	public void testC6_1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(eqv? #t #t)", T);
		equal(l,"(eqv? #f #f)", T);
		equal(l,"(eqv? 'obj1 'obj1)", T);
		equal(l,"(eqv? 1 1)", T);
		equal(l,"(eqv? 1.0 1.0)", T);
		equal(l,"(eqv? #\\a #\\a)", T);
		equal(l,"(eqv? '() '())", T);
		equal(l,"(let ((a '(1 2 3)))  (eqv? a a))", T);
		equal(l,"(let ((a '#(1 2 3))) (eqv? a a))", T);
		equal(l,"(let ((a \"123\"))   (eqv? a a))", T);
		equal(l,"(eqv? car car)", T);
		equal(l,"(let ((a (lambda (x) x))) (eqv? a a))", T);
		
		eq   (l,"(eqv? #f #f)", T);
		eq   (l,"(eqv? #f '(1))", F);
		eq   (l,"(eqv? #f 'a)", F);
		eq   (l,"(eqv? #f 1)", F);
		eq   (l,"(eqv? #f #\\a)", F);
		eq   (l,"(eqv? #f \"a\")", F);
		eq   (l,"(eqv? #f '#(1))", F);
		eq   (l,"(eqv? #f (current-input-port))", F);
		eq   (l,"(eqv? #f car)", F);
		
		eq   (l,"(eqv? '(1) #f)", F);
		//eq   (l,"(eqv? '(1) '(1))", T);
		eq   (l,"(eqv? '(1) 'a)", F);
		eq   (l,"(eqv? '(1) 1)", F);
		eq   (l,"(eqv? '(1) #\\a)", F);
		eq   (l,"(eqv? '(1) \"a\")", F);
		eq   (l,"(eqv? '(1) '#(1))", F);
		eq   (l,"(eqv? '(1) (current-input-port))", F);
		eq   (l,"(eqv? '(1) car)", F);
		
		eq   (l,"(eqv? 'a #f)", F);
		eq   (l,"(eqv? 'a '(1))", F);
		eq   (l,"(eqv? 'a 'a)", T);
		eq   (l,"(eqv? 'a 1)", F);
		eq   (l,"(eqv? 'a #\\a)", F);
		eq   (l,"(eqv? 'a \"a\")", F);
		eq   (l,"(eqv? 'a '#(1))", F);
		eq   (l,"(eqv? 'a (current-input-port))", F);
		eq   (l,"(eqv? 'a car)", F);
		
		eq   (l,"(eqv? 1 #f)", F);
		eq   (l,"(eqv? 1 '(1))", F);
		eq   (l,"(eqv? 1 'a)", F);
		eq   (l,"(eqv? 1 1)", T);
		eq   (l,"(eqv? 1 #\\a)", F);
		eq   (l,"(eqv? 1 \"a\")", F);
		eq   (l,"(eqv? 1 '#(1))", F);
		eq   (l,"(eqv? 1 (current-input-port))", F);
		eq   (l,"(eqv? 1 car)", F);
		
		eq   (l,"(eqv? #\\a #f)", F);
		eq   (l,"(eqv? #\\a '(1))", F);
		eq   (l,"(eqv? #\\a 'a)", F);
		eq   (l,"(eqv? #\\a 1)", F);
		eq   (l,"(eqv? #\\a #\\a)", T);
		eq   (l,"(eqv? #\\a \"a\")", F);
		eq   (l,"(eqv? #\\a '#(1))", F);
		eq   (l,"(eqv? #\\a (current-input-port))", F);
		eq   (l,"(eqv? #\\a car)", F);
		
		eq   (l,"(eqv? \"a\" #f)", F);
		eq   (l,"(eqv? \"a\" '(1))", F);
		eq   (l,"(eqv? \"a\" 'a)", F);
		eq   (l,"(eqv? \"a\" 1)", F);
		eq   (l,"(eqv? \"a\" #\\a)", F);
		//eq   (l,"(eqv? \"a\" \"a\")", T);
		eq   (l,"(eqv? \"a\" '#(1))", F);
		eq   (l,"(eqv? \"a\" (current-input-port))", F);
		eq   (l,"(eqv? \"a\" car)", F);
		
		eq   (l,"(eqv? '#(1) #f)", F);
		eq   (l,"(eqv? '#(1) '(1))", F);
		eq   (l,"(eqv? '#(1) 'a)", F);
		eq   (l,"(eqv? '#(1) 1)", F);
		eq   (l,"(eqv? '#(1) #\\a)", F);
		eq   (l,"(eqv? '#(1) \"a\")", F);
		//eq   (l,"(eqv? '#(1) '#(1))", T);
		eq   (l,"(eqv? '#(1) (current-input-port))", F);
		eq   (l,"(eqv? '#(1) car)", F);
		
		eq   (l,"(eqv? (current-input-port) #f)", F);
		eq   (l,"(eqv? (current-input-port) '(1))", F);
		eq   (l,"(eqv? (current-input-port) 'a)", F);
		eq   (l,"(eqv? (current-input-port) 1)", F);
		eq   (l,"(eqv? (current-input-port) #\\a)", F);
		eq   (l,"(eqv? (current-input-port) \"a\")", F);
		eq   (l,"(eqv? (current-input-port) '#(1))", F);
		//eq   (l,"(eqv? (current-input-port) (current-input-port))", T);
		eq   (l,"(eqv? (current-input-port) car)", F);
		
		eq   (l,"(eqv? car #f)", F);
		eq   (l,"(eqv? car '(1))", F);
		eq   (l,"(eqv? car 'a)", F);
		eq   (l,"(eqv? car 1)", F);
		eq   (l,"(eqv? car #\\a)", F);
		eq   (l,"(eqv? car \"a\")", F);
		eq   (l,"(eqv? car '#(1))", F);
		eq   (l,"(eqv? car (current-input-port))", F);
		eq   (l,"(eqv? car car)", T);
		
		eq   (l,"(eqv? (lambda (x) x) #f)", F);
		eq   (l,"(eqv? (lambda (x) x) '(1))", F);
		eq   (l,"(eqv? (lambda (x) x) 'a)", F);
		eq   (l,"(eqv? (lambda (x) x) 1)", F);
		eq   (l,"(eqv? (lambda (x) x) #\\a)", F);
		eq   (l,"(eqv? (lambda (x) x) \"a\")", F);
		eq   (l,"(eqv? (lambda (x) x) '#(1))", F);
		eq   (l,"(eqv? (lambda (x) x) (current-input-port))", F);
		eq   (l,"(eqv? (lambda (x) x) car)", F);
		
		eq   (l,"(eqv? '() #f)", F);
		eq   (l,"(eqv? '() '(1))", F);
		eq   (l,"(eqv? '() 'a)", F);
		eq   (l,"(eqv? '() 1)", F);
		eq   (l,"(eqv? '() #\\a)", F);
		eq   (l,"(eqv? '() \"a\")", F);
		eq   (l,"(eqv? '() '#(1))", F);
		eq   (l,"(eqv? '() (current-input-port))", F);
		eq   (l,"(eqv? '() car)", F);
		
		equal(l,"(eqv? #t #f)", F);
		equal(l,"(eqv? #f #t)", F);
		equal(l,"(eqv? 'obj1 'obj2)", F);
		equal(l,"(eqv? 1 1.0)", F);
		equal(l,"(eqv? 1 2)", F);
		equal(l,"(eqv? #\\a #\\b)", F);
		equal(l,"(eqv? (cons 1 2) (cons 1 2))", F);
		equal(l,"(eqv? (lambda (x) 1) (lambda (x) 2))", F);
		
		l.exec ("(define gen-counter" +
				"  (lambda ()" +
				"    (let ((n 0))" +
				"      (lambda () (set! n (+ n 1)) n))))");
		equal(l,"(let ((g (gen-counter)))" +
				"  (eqv? g g))", T);
		equal(l,"(eqv? (gen-counter) (gen-counter))", F);
		
		l.exec ("(define gen-loser" +
				"  (lambda ()" +
				"    (let ((n 0))" +
				"      (lambda () (set! n (+ n 1)) 27))))");
		equal(l,"(let ((g (gen-loser)))" +
				"  (eqv? g g))", T);
		equal(l,"(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))" +
				"         (g (lambda () (if (eqv? f g) 'g 'both))))" +
				"  (eqv? f g))", F);
		
		equal(l,"(let ((x '(a)))" +
				"  (eqv? x x))", T);
		
		equal(l,"(eqv? #t #t)", T);
		equal(l,"(eqv? #f #f)", T);
		equal(l,"(eqv? 'obj1 'obj1)", T);
		equal(l,"(eqv? 1 1)", T);
		equal(l,"(eqv? 1.0 1.0)", T);
		equal(l,"(eqv? #\\a #\\a)", T);
		equal(l,"(eqv? '() '())", T);
		equal(l,"(let ((a '(1 2 3)))  (eqv? a a))", T);
		equal(l,"(let ((a '#(1 2 3))) (eqv? a a))", T);
		equal(l,"(let ((a \"123\"))   (eqv? a a))", T);
		equal(l,"(eqv? car car)", T);
		equal(l,"(let ((a (lambda (x) x))) (eqv? a a))", T);
		
		equal(l,"(eq? #t #t)", T);
		equal(l,"(eq? #f #f)", T);
		equal(l,"(eq? 'obj1 'obj1)", T);
		//equal(l,"(eq? 1 1)", T);
		//equal(l,"(eq? 1.0 1.0)", T);
		//equal(l,"(eq? #\\a #\\a)", T);
		equal(l,"(eq? '() '())", T);
		equal(l,"(let ((a '(1 2 3)))  (eq? a a))", T);
		equal(l,"(let ((a '#(1 2 3))) (eq? a a))", T);
		equal(l,"(let ((a \"123\"))   (eq? a a))", T);
		equal(l,"(eq? car car)", T);
		equal(l,"(let ((a (lambda (x) x))) (eq? a a))", T);
		
		eq   (l,"(eq? #f #f)", T);
		eq   (l,"(eq? #f '(1))", F);
		eq   (l,"(eq? #f 'a)", F);
		eq   (l,"(eq? #f 1)", F);
		eq   (l,"(eq? #f #\\a)", F);
		eq   (l,"(eq? #f \"a\")", F);
		eq   (l,"(eq? #f '#(1))", F);
		eq   (l,"(eq? #f (current-input-port))", F);
		eq   (l,"(eq? #f car)", F);
		
		eq   (l,"(eq? '(1) #f)", F);
		//eq   (l,"(eq? '(1) '(1))", T);
		eq   (l,"(eq? '(1) 'a)", F);
		eq   (l,"(eq? '(1) 1)", F);
		eq   (l,"(eq? '(1) #\\a)", F);
		eq   (l,"(eq? '(1) \"a\")", F);
		eq   (l,"(eq? '(1) '#(1))", F);
		eq   (l,"(eq? '(1) (current-input-port))", F);
		eq   (l,"(eq? '(1) car)", F);
		
		eq   (l,"(eq? 'a #f)", F);
		eq   (l,"(eq? 'a '(1))", F);
		eq   (l,"(eq? 'a 'a)", T);
		eq   (l,"(eq? 'a 1)", F);
		eq   (l,"(eq? 'a #\\a)", F);
		eq   (l,"(eq? 'a \"a\")", F);
		eq   (l,"(eq? 'a '#(1))", F);
		eq   (l,"(eq? 'a (current-input-port))", F);
		eq   (l,"(eq? 'a car)", F);
		
		eq   (l,"(eq? 1 #f)", F);
		eq   (l,"(eq? 1 '(1))", F);
		eq   (l,"(eq? 1 'a)", F);
		//eq   (l,"(eq? 1 1)", T);
		eq   (l,"(eq? 1 #\\a)", F);
		eq   (l,"(eq? 1 \"a\")", F);
		eq   (l,"(eq? 1 '#(1))", F);
		eq   (l,"(eq? 1 (current-input-port))", F);
		eq   (l,"(eq? 1 car)", F);
		
		eq   (l,"(eq? #\\a #f)", F);
		eq   (l,"(eq? #\\a '(1))", F);
		eq   (l,"(eq? #\\a 'a)", F);
		eq   (l,"(eq? #\\a 1)", F);
		//eq   (l,"(eq? #\\a #\\a)", T);
		eq   (l,"(eq? #\\a \"a\")", F);
		eq   (l,"(eq? #\\a '#(1))", F);
		eq   (l,"(eq? #\\a (current-input-port))", F);
		eq   (l,"(eq? #\\a car)", F);
		
		eq   (l,"(eq? \"a\" #f)", F);
		eq   (l,"(eq? \"a\" '(1))", F);
		eq   (l,"(eq? \"a\" 'a)", F);
		eq   (l,"(eq? \"a\" 1)", F);
		eq   (l,"(eq? \"a\" #\\a)", F);
		//eq   (l,"(eq? \"a\" \"a\")", T);
		eq   (l,"(eq? \"a\" '#(1))", F);
		eq   (l,"(eq? \"a\" (current-input-port))", F);
		eq   (l,"(eq? \"a\" car)", F);
		
		eq   (l,"(eq? '#(1) #f)", F);
		eq   (l,"(eq? '#(1) '(1))", F);
		eq   (l,"(eq? '#(1) 'a)", F);
		eq   (l,"(eq? '#(1) 1)", F);
		eq   (l,"(eq? '#(1) #\\a)", F);
		eq   (l,"(eq? '#(1) \"a\")", F);
		//eq   (l,"(eq? '#(1) '#(1))", T);
		eq   (l,"(eq? '#(1) (current-input-port))", F);
		eq   (l,"(eq? '#(1) car)", F);
		
		eq   (l,"(eq? (current-input-port) #f)", F);
		eq   (l,"(eq? (current-input-port) '(1))", F);
		eq   (l,"(eq? (current-input-port) 'a)", F);
		eq   (l,"(eq? (current-input-port) 1)", F);
		eq   (l,"(eq? (current-input-port) #\\a)", F);
		eq   (l,"(eq? (current-input-port) \"a\")", F);
		eq   (l,"(eq? (current-input-port) '#(1))", F);
		//eq   (l,"(eq? (current-input-port) (current-input-port))", T);
		eq   (l,"(eq? (current-input-port) car)", F);
		
		eq   (l,"(eq? car #f)", F);
		eq   (l,"(eq? car '(1))", F);
		eq   (l,"(eq? car 'a)", F);
		eq   (l,"(eq? car 1)", F);
		eq   (l,"(eq? car #\\a)", F);
		eq   (l,"(eq? car \"a\")", F);
		eq   (l,"(eq? car '#(1))", F);
		eq   (l,"(eq? car (current-input-port))", F);
		eq   (l,"(eq? car car)", T);
		
		eq   (l,"(eq? (lambda (x) x) #f)", F);
		eq   (l,"(eq? (lambda (x) x) '(1))", F);
		eq   (l,"(eq? (lambda (x) x) 'a)", F);
		eq   (l,"(eq? (lambda (x) x) 1)", F);
		eq   (l,"(eq? (lambda (x) x) #\\a)", F);
		eq   (l,"(eq? (lambda (x) x) \"a\")", F);
		eq   (l,"(eq? (lambda (x) x) '#(1))", F);
		eq   (l,"(eq? (lambda (x) x) (current-input-port))", F);
		eq   (l,"(eq? (lambda (x) x) car)", F);
		
		eq   (l,"(eq? '() #f)", F);
		eq   (l,"(eq? '() '(1))", F);
		eq   (l,"(eq? '() 'a)", F);
		eq   (l,"(eq? '() 1)", F);
		eq   (l,"(eq? '() #\\a)", F);
		eq   (l,"(eq? '() \"a\")", F);
		eq   (l,"(eq? '() '#(1))", F);
		eq   (l,"(eq? '() (current-input-port))", F);
		eq   (l,"(eq? '() car)", F);
		
		equal(l,"(eq? #t #f)", F);
		equal(l,"(eq? #f #t)", F);
		equal(l,"(eq? 'obj1 'obj2)", F);
		equal(l,"(eq? 1 1.0)", F);
		equal(l,"(eq? 1 2)", F);
		equal(l,"(eq? #\\a #\\b)", F);
		equal(l,"(eq? (cons 1 2) (cons 1 2))", F);
		equal(l,"(eq? (lambda (x) 1) (lambda (x) 2))", F);
		
		equal(l,"(equal? 'a 'a)", T);
		equal(l,"(equal? '(a) '(a))", T);
		equal(l,"(equal? '(a (b) c)" +
				"        '(a (b) c))", T);
		equal(l,"(equal? \"abc\" \"abc\")", T);
		equal(l,"(equal? 2 2)", T);
		equal(l,"(equal? (make-vector 5 'a)" +
				"        (make-vector 5 'a))", T);
	}
	
	public void testC6_2_5() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(complex? 3+4i)", T);
		equal(l,"(complex? 3)", T);
		equal(l,"(real? 3)", T);
		equal(l,"(real? -2.5+0.0i)", F);
		equal(l,"(real? #e1e10)", T);
		equal(l,"(rational? 6/10)", T);
		equal(l,"(rational? 6/3)", T);
		equal(l,"(integer? 3+0i)", T);
		equal(l,"(integer? 3.0)", T);
		equal(l,"(integer? 8/4)", T);
		equal(l,"(max 3 4)", newZ(4));
		equal(l,"(max 3.9 4)", newR(4.0));
		equal(l,"(+ 3 4)", newZ(7));
		equal(l,"(+ 3)", newZ(3));
		equal(l,"(+)", newZ(0));
		equal(l,"(* 4)", newZ(4));
		equal(l,"(*)", newZ(1));
		equal(l,"(- 3 4)", newZ(-1));
		equal(l,"(- 3 4 5)", newZ(-6));
		equal(l,"(- 3)", newZ(-3));
		equal(l,"(/ 3 4 5)", newQ(3, 20));
		equal(l,"(/ 3)", newQ(1, 3));
		equal(l,"(abs -7)", newZ(7));
		equal(l,"(= 4 (+ (* 5 (quotient 4 5))" +
				"      (remainder 4 5)))", T);
		equal(l,"(modulo 13 4)", newZ(1));
		equal(l,"(remainder 13 4)", newZ(1));
		equal(l,"(modulo -13 4)", newZ(3));
		equal(l,"(remainder -13 4)", newZ(-1));
		equal(l,"(modulo 13 -4)", newZ(-3));
		equal(l,"(remainder 13 -4)", newZ(1));
		equal(l,"(modulo -13 -4)", newZ(-1));
		equal(l,"(remainder -13 -4)", newZ(-1));
		equal(l,"(remainder -13 -4.0)", newR(-1.0));
		equal(l,"(gcd 32 -36)", newZ(4));
		equal(l,"(gcd)", newZ(0));
		equal(l,"(lcm 32 -36)", newZ(288));
		equal(l,"(lcm 32.0 -36)", newR(288.0));
		equal(l,"(lcm)", newZ(1));
		equal(l,"(numerator (/ 6 4))", newZ(3));
		equal(l,"(denominator (/ 6 4))", newZ(2));
		equal(l,"(denominator" +
				"  (exact->inexact (/ 6 4)))", newR(2.0));
		equal(l,"(floor -4.3)", newR(-5.0));
		equal(l,"(ceiling -4.3)", newR(-4.0));
		equal(l,"(truncate -4.3)", newR(-4.0));
		equal(l,"(round -4.3)", newR(-4.0));
		equal(l,"(floor 3.5)", newR(3.0));
		equal(l,"(ceiling 3.5)", newR(4.0));
		equal(l,"(truncate 3.5)", newR(3.0));
		equal(l,"(round 3.5)", newR(4.0));
		equal(l,"(round 7/2)", newZ(4));
		equal(l,"(round 7)", newZ(7));
	}
	
	public void testC6_2_6() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define (chk1? number radix)" +
				"  (eqv? number" +
				"        (string->number (number->string number" +
				"                                        radix)" +
				"                        radix)))");
		equal(l,"(chk1? 1000 2)", T);
		equal(l,"(chk1? 1000 8)", T);
		equal(l,"(chk1? 1000 10)", T);
		equal(l,"(chk1? 1000 16)", T);
		equal(l,"(chk1? 100000000000000000000000 2)", T);
		equal(l,"(chk1? 100000000000000000000000 8)", T);
		equal(l,"(chk1? 100000000000000000000000 10)", T);
		equal(l,"(chk1? 100000000000000000000000 16)", T);
		equal(l,"(chk1? 1/2 10)", T);
		equal(l,"(chk1? 1.5 10)", T);
		equal(l,"(chk1? 1+i 10)", T);
		equal(l,"(string->number \"100\")", newZ(100));
		equal(l,"(string->number \"100\" 16)", newZ(256));
		equal(l,"(string->number \"1e2\")", newR(100.0));
		//equal(l,"(string->number \"15##\")", newR(1500.0));
	}
	
	public void testC6_3_1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(not #t)", F);
		equal(l,"(not 3)", F);
		equal(l,"(not (list 3))", F);
		equal(l,"(not #f)", T);
		equal(l,"(not '())", F);
		equal(l,"(not (list))", F);
		equal(l,"(not 'nil)", F);
		equal(l,"(boolean? #f)", T);
		equal(l,"(boolean? 0)", F);
		equal(l,"(boolean? '())", F);
	}
	
	public void testC6_3_2() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define x (list 'a 'b 'c))");
		l.exec ("(define y x)");
		equal(l,"y", list(sym("a"), sym("b"), sym("c")));
		equal(l,"(list? y)", T);
		equal(l,"(set-cdr! x 4)", Undef.UNDEF);
		equal(l,"x", cons(sym("a"), 4));
		equal(l,"(eqv? x y)", T);
		equal(l,"y", cons(sym("a"), 4));
		equal(l,"(list? y)", F);
		equal(l,"(set-cdr! x x)", Undef.UNDEF);
		equal(l,"(list? x)", F);
		equal(l,"(pair? '(a . b))", T);
		equal(l,"(pair? '(a b c))", T);
		equal(l,"(pair? '())", F);
		equal(l,"(pair? '#(a b))", F);
		equal(l,"(cons 'a '())", list(sym("a")));
		equal(l,"(cons '(a) '(b c d))",
				list(list(sym("a")), sym("b"), sym("c"), sym("d")));
		equal(l,"(cons \"a\" '(b c))",
				list(str("a"), sym("b"), sym("c")));
		equal(l,"(cons '(a b) 'c)",
				cons(list(sym("a"), sym("b")), sym("c")));
		equal(l,"(car '(a b c))", sym("a"));
		equal(l,"(car '((a) b c d))", list(sym("a")));
		equal(l,"(car '(1 . 2))", newZ(1));
		lperr(l,"(car '())");
		equal(l,"(cdr '((a) b c d))",
				list(sym("b"), sym("c"), sym("d")));
		equal(l,"(cdr '(1 . 2))", newZ(2));
		lperr(l,"(cdr '())");
		equal(l,"(null? '())", T);
		equal(l,"(null? '(a))", F);
		equal(l,"(null? '(a . b))", F);
		equal(l,"(list? '(a b c))", T);
		equal(l,"(list? '())", T);
		equal(l,"(list? '(a . b))", F);
		//equal(l,"(let ((x (list 'a)))" +
		//		"  (set-cdr! x x)" +
		//		"  (list? x))", F);
		equal(l,"(list 'a (+ 3 4) 'c)", list(sym("a"), 7, sym("c")));
		equal(l,"(list)", Nil.NIL);
		equal(l,"(length '(a b c))", newZ(3));
		equal(l,"(length '(a (b) (c d e)))", newZ(3));
		equal(l,"(length '())", newZ(0));
		equal(l,"(append '(x) '(y))", list(sym("x"), sym("y")));
		equal(l,"(append '(a) '(b c d))",
				list(sym("a"), sym("b"), sym("c"), sym("d")));
		equal(l,"(append '(a (b)) '((c)))",
				list(sym("a"), list(sym("b")), list(sym("c"))));
		equal(l,"(append '(a b) '(c . d))",
				listDot(sym("d"), sym("a"), sym("b"), sym("c")));
		equal(l,"(append '() 'a)", sym("a"));
		equal(l,"(reverse '(a b c))", list(sym("c"), sym("b"), sym("a")));
		equal(l,"(reverse '(a (b c) d (e (f))))",
				list(list(sym("e"), list(sym("f"))), sym("d"),
						list(sym("b"), sym("c")), sym("a")));
		equal(l,"(list-ref '(a b c d) 2)", sym("c"));
		equal(l,"(list-ref '(a b c d)" +
				"  (inexact->exact (round 1.8)))", sym("c"));
		equal(l,"(memq 'a '(a b c))", list(sym("a"), sym("b"), sym("c")));
		equal(l,"(memq 'b '(a b c))", list(sym("b"), sym("c")));
		equal(l,"(memq 'a '(b c d))", F);
		equal(l,"(memq (list 'a) '(b (a) c))", F);
		equal(l,"(member (list 'a) '(b (a) c))",
				list(list(sym("a")), sym("c")));
		equal(l,"(memv 101 '(100 101 102))", list(101, 102));
		l.exec ("(define e '((a 1) (b 2) (c 3)))");
		equal(l,"(assq 'a e)", list(sym("a"), 1));
		equal(l,"(assq 'b e)", list(sym("b"), 2));
		equal(l,"(assq 'd e)", F);
		equal(l,"(assq (list 'a) '(((a)) ((b)) ((c))))", F);
		equal(l,"(assoc (list 'a) '(((a)) ((b)) ((c))))",
				list(list(sym("a"))));
		equal(l,"(assv 5 '((2 3) (5 7) (11 13)))", list(5, 7));
	}
	
	public void testC6_3_3() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(symbol? 'foo)", T);
		equal(l,"(symbol? (car '(a b)))", T);
		equal(l,"(symbol? \"bar\")", F);
		equal(l,"(symbol? 'nil)", T);
		equal(l,"(symbol? '())", F);
		equal(l,"(symbol? #f)", F);
		equal(l,"(not 'nil)", F);
		equal(l,"(symbol->string 'flying-fish)", str("flying-fish"));
		equal(l,"(symbol->string" +
				"  (string->symbol \"Malvina\"))", str("Malvina"));
	}
	
	public void testC6_3_6() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(vector 'a 'b 'c)", vec(sym("a"), sym("b"), sym("c")));
		equal(l,"(vector-ref '#(1 1 2 3 5 8 13 21) 5)", newZ(8));
		equal(l,"(vector-ref '#(1 1 2 3 5 8 13 21)" +
				"  (let ((i (round (* 2 (acos -1)))))" +
				"    (if (inexact? i)" +
				"        (inexact->exact i)" +
				"        i)))", newZ(13));
		equal(l,"(let ((vec (vector 0 '(2 2 2 2) \"Anna\")))" +
				"  (vector-set! vec 1 '(\"Sue\" \"Sue\"))" +
				"  vec)",
				vec(0, list(str("Sue"), str("Sue")), str("Anna")));
		equal(l,"(vector->list '#(dah dah didah))",
				list(sym("dah"), sym("dah"), sym("didah")));
		equal(l,"(list->vector '(dididit dah))",
				vec(sym("dididit"), sym("dah")));
	}
	
	public void testC6_4() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(procedure? car)", T);
		equal(l,"(procedure? 'car)", F);
		equal(l,"(procedure? (lambda (x) (* x x)))", T);
		equal(l,"(procedure? '(lambda (x) (* x x)))", F);
		equal(l,"(call-with-current-continuation procedure?)", T);
		equal(l,"(apply + (list 3 4))", newZ(7));
		l.exec ("(define compose" +
				"  (lambda (f g)" +
				"    (lambda args" +
				"      (f (apply g args)))))");
		equal(l,"(map cadr '((a b) (d e) (g h)))",
				list(sym("b"), sym("e"), sym("h")));
		equal(l,"(map (lambda (n) (expt n n))" +
				"     '(1 2 3 4 5))", list(1, 4, 27, 256, 3125));
		equal(l,"(map + '(1 2 3) '(4 5 6))", list(5, 7, 9));
		equal(l,"(let ((count 0))" +
				"  (map (lambda (ignored)" +
				"         (set! count (+ count 1))" +
				"         count)" +
				"       '(a b)))", list(1, 2));
		equal(l,"(let ((v (make-vector 5)))" +
				"  (for-each (lambda (i)" +
				"              (vector-set! v i (* i i)))" +
				"            '(0 1 2 3 4))" +
				"  v)", vec(0, 1, 4, 9, 16));
		equal(l,"(force (delay (+ 1 2)))", newZ(3));
		equal(l,"(let ((p (delay (+ 1 2))))" +
				"  (list (force p) (force p)))", list(3, 3));
		l.exec ("(define a-stream" +
				"  (letrec ((next" +
				"            (lambda (n)" +
				"              (cons n (delay (next (+ n 1)))))))" +
				"    (next 0)))");
		l.exec ("(define head car)");
		l.exec ("(define tail" +
				"  (lambda (stream) (force (cdr stream))))");
		equal(l,"(head (tail (tail a-stream)))", newZ(2));
		l.exec ("(define count 0)");
		l.exec ("(define p" +
				"  (delay (begin (set! count (+ count 1))" +
				"                (if (> count x)" +
				"                    count" +
				"                    (force p)))))");
		l.exec ("(define x 5)");
		equal(l,"(force p)", newZ(6));
		equal(l,"(begin (set! x 10)" +
				"       (force p))", newZ(6));
		equal(l,"(call-with-current-continuation" +
				"  (lambda (exit)" +
				"    (for-each (lambda (x)" +
				"                (if (negative? x)" +
				"                    (exit x)))" +
				"                '(54 0 37 -3 245 19))" +
				"    #t))", newZ(-3));
		l.exec ("(define list-length" +
				"  (lambda (obj)" +
				"    (call-with-current-continuation" +
				"      (lambda (return)" +
				"        (letrec ((r" +
				"                  (lambda (obj)" +
				"                    (cond ((null? obj) 0)" +
				"                          ((pair? obj)" +
				"                           (+ (r (cdr obj)) 1))" +
				"                          (else (return #f))))))" +
				"          (r obj))))))");
		equal(l,"(list-length '(1 2 3 4))", newZ(4));
		equal(l,"(list-length '(a b . c))", F);
		equal(l,"(call-with-values (lambda () (values 4 5))" +
				"                  (lambda (a b) b))", newZ(5));
		equal(l,"(call-with-values * -)", newZ(-1));
	}
	
	public void testC6_5() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(eval '(* 7 3) (scheme-report-environment 5))",
				newZ(21));
		equal(l,"(let ((f (eval '(lambda (f x) (f x x))" +
				"               (null-environment 5))))" +
				"  (f + 10))", newZ(20));
	}
	
}
