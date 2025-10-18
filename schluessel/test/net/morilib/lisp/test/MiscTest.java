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

import net.morilib.lisp.Scheme;

public class MiscTest extends TCSubr {
	
	public void testEval1() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x 1)");
		l.input("(define cnt #f)");
		l.input("(+ 1 (call/cc (lambda (k) (set! cnt k) 1)))");
		eqi  (l,"(eval '(+ x 2) (interaction-environment))", 3);
		eqi  (l,"(eval '(list 1 (cnt 3)) (interaction-environment))", 4);
		eqi  (l,"(eval '(if (= x 1) 3) (interaction-environment))", 3);
		lperr(l,"(eval '(+ x 2))");
	}
	
	public void testEval2() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x 1)");
		l.input("(define cnt #f)");
		l.input("(+ 1 (call/cc (lambda (k) (set! cnt k) 1)))");
		equal(l,"(eval '(list 1 2 3) " +
				"  (scheme-report-environment 5))", list(1, 2 ,3));
		eqi  (l,"(eval '(if (= 1 1) 3) " +
				"  (scheme-report-environment 5))", 3);
		lperr(l,"(eval '(+ x 2) " +
				"  (scheme-report-environment 5))");
		lperr(l,"(eval '(list 1 (cnt 3)) " +
				"  (scheme-report-environment 5))");
		lperr(l,"(eval '(+ x 2))");
	}
	
	public void testEval3() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x 1)");
		l.input("(define cnt #f)");
		l.input("(+ 1 (call/cc (lambda (k) (set! cnt k) 1)))");
		eqi  (l,"(eval '(if 1 3) " +
				"  (null-environment 5))", 3);
		lperr(l,"(eval '(list 1 2 3) " +
				"  (null-environment 5))");
		lperr(l,"(eval '(+ x 2) " +
				"  (null-environment 5))");
		lperr(l,"(eval '(list 1 (cnt 3)) " +
				"  (null-environment 5))");
		lperr(l,"(eval '(+ x 2))");
	}
	
	public void testUse1() {
		Scheme l = Scheme.newInstance();
		
		l.input("(add-use-path net.morilib.lisp.test)");
		equal(l,"*use-path*", list(sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib")));
		l.input("(add-use-path aaaaa)");
		equal(l,"*use-path*", list(sym("aaaaa"),
				sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib")));
		l.input("(add-use-path aaa #t)");
		equal(l,"*use-path*", list(sym("aaaaa"),
				sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib"),
				sym("aaa")));
		
		l.input("(use testlib)");
		eqi  (l,"testok", 1);
		equal(l,"(tests1 1 2)", list(1, 2, 1));
		eqi  (l,"(1+ 1)", 2);
		eqi  (l,"(1- 1)", 0);
	}
	
	public void testUse2() {
		Scheme l = Scheme.newInstance();
		
		l.input("(add-use-path net.morilib.lisp.test)");
		equal(l,"*use-path*", list(sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib")));
		l.input("(add-use-path aaaaa)");
		equal(l,"*use-path*", list(sym("aaaaa"),
				sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib")));
		l.input("(add-use-path aaa #t)");
		equal(l,"*use-path*", list(sym("aaaaa"),
				sym("net.morilib.lisp.test"),
				sym("net.morilib.lisp.lib"),
				sym("aaa")));
		
		l.input("(use net.morilib.lisp.test.testlib)");
		eqi  (l,"testok", 1);
		equal(l,"(tests1 1 2)", list(1, 2, 1));
		eqi  (l,"(1+ 1)", 2);
		eqi  (l,"(1- 1)", 0);
	}
	
	public void testMacro1() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-macro mc1 " +
				"  (lambda (x) `(+ 1 ,x)))");
		equal(l,"(macroexpand   '(mc1 2))", list(sym("+"), 1, 2));
		equal(l,"(macroexpand-1 '(mc1 2))", list(sym("+"), 1, 2));
		eqi  (l,"(mc1 2)", 3);
	}
	
	public void testMacro2() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-macro mc1 " +
				"  (lambda (r x)" +
				"    (if (null? x) `(list ,@r)" +
				"        `(mc1 ,(cons (car x) r) ,(cdr x)))))");
		equal(l,"(macroexpand   '(mc1 () (1 2)))", list(
				sym("list"), 2, 1));
		equal(l,"(macroexpand-1  '(mc1 () (1 2)))", list(
				sym("mc1"), list(1), list(2)));
		equal(l,"(mc1 () (1 2))", list(2, 1));
	}
	
	public void testMacro3() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-macro mc1 " +
				"  (lambda (x) `(+ 1 ,x)))");
		l.input("(define-macro mc2 " +
				"  (lambda (x) `(mc1 (+ 1 ,x))))");
		equal(l,"(macroexpand   '(mc2 2))", list(
				sym("+"), 1, list(sym("+"), 1, 2)));
		equal(l,"(macroexpand-1 '(mc2 2))", list(
				sym("mc1"), list(sym("+"), 1, 2)));
		eqi  (l,"(mc2 2)", 4);
	}
	
	public void testMacro4() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-macro mc1 " +
				"  (lambda (x)" +
				"    `(define-macro ,x" +
				"       (lambda (y) `(+ 1 ,y)))))");
		l.input("(mc1 zzz)");
		equal(l,"(macroexpand   '(zzz 2))", list(sym("+"), 1, 2));
		equal(l,"(macroexpand-1 '(zzz 2))", list(sym("+"), 1, 2));
		eqi  (l,"(zzz 2)", 3);
	}
	
}
