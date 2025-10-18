/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/16
 */
public class R6RSIdentifierSyntaxTest extends TCSubr {

	public void testIdentifierSyntax1() {
		Scheme l = Scheme.newInstance();

		l.input("(define p (cons 4 5))");
		l.input("(define-syntax p.car (identifier-syntax (car p)))");
		eqi  (l,"p.car", 4);
		lperr(l,"(set! p.car 15)");
	}

	public void testIdentifierSyntax2() {
		Scheme l = Scheme.newInstance();

		l.input("(define q (cons 4 5))");
		l.input("(define-syntax p (identifier-syntax q))");
		l.input("(define-syntax p.car (identifier-syntax (car p)))");
		eqi  (l,"p.car", 4);
		lperr(l,"(set! p.car 15)");
	}

	public void testIdentifierSyntax3() {
		Scheme l = Scheme.newInstance();

		l.input("(define q (cons 4 5))");
		l.input("(define-syntax q.car (identifier-syntax (car q)))");
		l.input("(define-syntax p.car (identifier-syntax q.car))");
		eqi  (l,"p.car", 4);
		lperr(l,"(set! p.car 15)");
	}

	public void testIdentifierSyntax4() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(let ((q (cons 4 5)))" +
				"  (define-syntax q.car (identifier-syntax (car q)))" +
				"  (define-syntax p.car (identifier-syntax q.car))" +
				"  (set! x p.car))");
		eqi  (l,"x", 4);
	}

	public void testIdentifierSyntax5() {
		Scheme l = Scheme.newInstance();

		l.input("(define p (cons 4 5))");
		l.input("(define-syntax p.car" +
				"  (identifier-syntax" +
				"    (_ (car p))" +
				"    ((set! _ e) (set-car! p e))))");
		l.input("(set! p.car 15)");
		eqi  (l,"p.car", 15);
		equal(l,"p", cons(15, 5));
	}

	public void testIdentifierSyntax6() {
		Scheme l = Scheme.newInstance();

		l.input("(define-syntax mc1" +
				"  (identifier-syntax" +
				"    (_ (car p))" +
				"    ((set! _ (e ...)) (list e ...))))");
		equal(l,"(set! mc1 (72 83 91))", list(72, 83, 91));
	}

	public void testIdentifierSyntax7() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(let ()" +
				"  (define-syntax mc1" +
				"    (identifier-syntax" +
				"      (_ (car p))" +
				"      ((set! _ (e ...)) (list e ...))))" +
				"  (set! x (set! mc1 (72 83 91))))");
		equal(l,"x", list(72, 83, 91));
	}

	public void testIdentifierSyntaxLet1() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(let ((q (cons 4 5)))" +
				"  (let-syntax" +
				"      ((q.car (identifier-syntax (car q)))" +
				"       (p.car (identifier-syntax q.car)))" +
				"    (set! x p.car)))");
		eqi  (l,"x", 4);
	}

	public void testIdentifierSyntaxLet2() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(let-syntax" +
				"    ((mc1" +
				"      (identifier-syntax" +
				"        (_ (car p))" +
				"        ((set! _ (e ...)) (list e ...)))))" +
				"  (set! x (set! mc1 (72 83 91))))");
		equal(l,"x", list(72, 83, 91));
	}

	public void testIdentifierSyntaxLet3() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(define p (cons 4 5))");
		l.input("(let ((p (cons 4 5)))" +
				"  (let-syntax" +
				"      ((p.car" +
				"         (identifier-syntax" +
				"         (_ (car p))" +
				"         ((set! _ e) (set-car! p e)))))" +
				"    (set! p.car 15)" +
				"    (set! x p)))");
		equal(l,"x", cons(15, 5));
		equal(l,"p", cons( 4, 5));
	}

	public void testIdentifierSyntaxLet4() {
		Scheme l = Scheme.newInstance();

		l.input("(define p (cons 4 5))");
		l.input("(let ()" +
				"  (let-syntax" +
				"      ((p.car" +
				"         (identifier-syntax" +
				"         (_ (car p))" +
				"         ((set! _ e) (set-car! p e)))))" +
				"    (set! p.car 15)))");
		equal(l,"p", cons(15, 5));
	}

	public void testIdentifierSyntaxLetrec1() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(let ((q (cons 4 5)))" +
				"  (letrec-syntax" +
				"      ((q.car (identifier-syntax (car q)))" +
				"       (p.car (identifier-syntax q.car)))" +
				"    (set! x p.car)))");
		eqi  (l,"x", 4);
	}

	public void testIdentifierSyntaxLetrec2() {
		Scheme l = Scheme.newInstance();

		l.input("(define x #f)");
		l.input("(letrec-syntax" +
				"    ((mc1" +
				"      (identifier-syntax" +
				"        (_ (car p))" +
				"        ((set! _ (e ...)) (list e ...)))))" +
				"  (set! x (set! mc1 (72 83 91))))");
		equal(l,"x", list(72, 83, 91));
	}

}
