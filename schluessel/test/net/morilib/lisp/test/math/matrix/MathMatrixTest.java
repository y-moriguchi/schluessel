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
package net.morilib.lisp.test.math.matrix;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/29
 */
public class MathMatrixTest extends TCSubr {

	public void testIsMatrix() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 2 3)" +
				"    (4 5 6)" +
				"    (7 8 9))))");
		equal(l,"(matrix? x)", T);
		equal(l,"(matrix? 1)", F);
	}

	public void testListToMatrix() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 0 3)" +
				"    (2 1 -1 1)" +
				"    (3 -1 -1 2)" +
				"    (-1 2 3 -1))))");
		l.exec ("(define x (list->matrix " +
				"  '((1 1 0 3)" +
				"    (2 1 -1 1)" +
				"    (3 -1 -1 2))))");
		lperr(l,"(define x (list->matrix " +
				"  '((1 1 0 3)" +
				"    (2 1 -1 1)" +
				"    (3 -1 -1)" +
				"    (-1 2 3 -1))))");
	}

	public void testLuDecompose() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 0 3)" +
				"    (2 1 -1 1)" +
				"    (3 -1 -1 2)" +
				"    (-1 2 3 -1))))");
		equal(l,"(receive (p l u) (lu-decompose x)" +
				"  (display p)" +
				"  (display l)" +
				"  (display u)" +
				"  (= (* p (* l u)) x))", T);
	}

	public void testMakeMatrix() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 1 1)" +
				"    (1 1 1 1)" +
				"    (1 1 1 1)" +
				"    (1 1 1 1))))");
		l.exec ("(define y (list->matrix " +
				"  '((0 0)" +
				"    (0 0))))");
		equal(l,"(= x (make-matrix 4 4 1))", T);
		equal(l,"(= y (make-matrix 2 2))", T);
		lperr(l,"(make-matrix 0 1)");
		lperr(l,"(make-matrix 1 0)");
	}

	public void testDeterminant() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((2 1 -1 -1)" +
				"    (-2 -3 1 2)" +
				"    (-2 -1 3 1)" +
				"    (-1 -4 1 2))))");
		eqi  (l,"(matrix-determinant x)", 2);
	}

	public void testMatrixEqual() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 0 3)" +
				"    (2 1 -1 1)" +
				"    (3 -1 -1 2)" +
				"    (-1 2 3 -1))))");
		l.exec ("(define y (list->matrix " +
				"  '((1 1 0 3.0)" +
				"    (2 1 -1 1.0)" +
				"    (3 -1 -1 2)" +
				"    (-1 2 3 -1))))");
		equal(l,"(matrix= x y)", T);
		equal(l,"(eqv? x y)", F);
	}

	public void testMatrixInvert() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 0 1 -1)" +
				"    (0 1 -1 3 0)" +
				"    (-2 -1 1 2 3)" +
				"    (1 0 2 -1 -1)" +
				"    (-1 -2 -3 -7 1))))");
		l.exec ("(define xi (list->matrix " +
				"  '((-6 15 1 13 4)" +
				"    (11 -18 0 -15 -4)" +
				"    (2 -4 0 -3 -1)" +
				"    (-3 5 0 4 1)" +
				"    (1 2 1 2 1))))");
		l.exec ("(define y (list->matrix " +
				"  '((2 3 2 1)" +
				"    (4 2 -1 1)" +
				"    (-2 -1 -1 -2)" +
				"    (2 1 2 3))))");
		l.exec ("(define z (list->matrix " +
				"  '((2 3 2 1)" +
				"    (4 2 -1 1)" +
				"    (-2 -1 -1 -2))))");
		equal(l,"(= (matrix-invert x) xi)", T);
		lperr(l,"(matrix-invert y)");
		lperr(l,"(matrix-invert z)");
	}

	public void testMatrixMinus() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 2 3)" +
				"    (4 5 6)" +
				"    (7 8 9))))");
		l.exec ("(define y (list->matrix " +
				"  '((2 4 6)" +
				"    (3 6 9)" +
				"    (4 8 12))))");
		equal(l,"(= (matrix- y x)" +
				"  (list->matrix" +
				"    '((1 2 3)" +
				"      (-1 1 3)" +
				"      (-3 0 3))))", T);
		equal(l,"(= (- y x)" +
				"  (list->matrix" +
				"    '((1 2 3)" +
				"      (-1 1 3)" +
				"      (-3 0 3))))", T);
		lperr(l,"(matrix- x (make-matrix 2 3))");
		lperr(l,"(- x (make-matrix 2 3))");
	}

	public void testMatrixMul() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((2 4)" +
				"    (3 6)" +
				"    (4 8))))");
		l.exec ("(define y (list->matrix " +
				"  '((1 2 3)" +
				"    (4 5 6))))");
		equal(l,"(= (matrix* x y)" +
				"  (list->matrix" +
				"    '((18 24 30)" +
				"      (27 36 45)" +
				"      (36 48 60))))", T);
		equal(l,"(= (* x y)" +
				"  (list->matrix" +
				"    '((18 24 30)" +
				"      (27 36 45)" +
				"      (36 48 60))))", T);
		lperr(l,"(matrix* x (make-matrix 4 4))");
		lperr(l,"(* x (make-matrix 4 4))");
	}

	public void testMatrixPlus() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 2 3)" +
				"    (4 5 6)" +
				"    (7 8 9))))");
		l.exec ("(define y (list->matrix " +
				"  '((2 4 6)" +
				"    (3 6 9)" +
				"    (4 8 12))))");
		equal(l,"(= (matrix+ y x)" +
				"  (list->matrix" +
				"    '((3 6 9)" +
				"      (7 11 15)" +
				"      (11 16 21))))", T);
		equal(l,"(= (+ y x)" +
				"  (list->matrix" +
				"    '((3 6 9)" +
				"      (7 11 15)" +
				"      (11 16 21))))", T);
		lperr(l,"(matrix+ x (make-matrix 2 3))");
		lperr(l,"(+ x (make-matrix 2 3))");
	}

	public void testSolveLinearEquation() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x (list->matrix " +
				"  '((1 1 2 1)" +
				"    (1 1 3 2)" +
				"    (2 -2 2 -1)" +
				"    (-1 1 0 1))))");
		l.exec ("(define y (list->matrix " +
				"  '((2 3 2 1)" +
				"    (4 2 -1 1)" +
				"    (-2 -1 -1 -2)" +
				"    (2 1 2 3))))");
		equal(l,"(= (solve-linear-equation" +
				"        x (number-vector -1 2 -1 1))" +
				"      (number-vector 1 -3 -2 5))", T); 
		lperr(l,"(solve-linear-equation y" +
				"  (number-vector 1 2 -1 1))");
		lperr(l,"(solve-linear-equation" +
				"  (list->matrix" +
				"    '((1 2 3)" +
				"      (3 4 5)))" +
				"  (number-vector 1 2 3))"); 
		lperr(l,"(solve-linear-equation x" +
				"  (number-vector 1 2 3))"); 
	}

	public void testMakeNumberVector() {
		Scheme l = Scheme.newInstance();

		equal(l,"(= (make-number-vector 4 1)" +
				"      (number-vector 1 1 1 1))", T);
		equal(l,"(= (make-number-vector 4)" +
				"      (number-vector 0 0 0 0))", T);
	}

}
