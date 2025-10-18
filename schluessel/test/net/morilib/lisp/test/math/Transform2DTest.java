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
package net.morilib.lisp.test.math;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/01
 */
public class Transform2DTest extends TCSubr {

	public void testLinearOp() {
		Scheme l = Scheme.newInstance();

		// l1     l2
		// (1 2)  (5 6)
		// (3 4)  (7 8)
		l.input("(define l1 (make-linear-transform-2d 1 2 3 4))");
		l.input("(define l2 (make-linear-transform-2d 2 3 4 5))");
		l.input("(define l3 (make-linear-transform-2d 3 5 7 9))");
		l.input("(define l4 (make-linear-transform-2d 1 1 1 1))");
		l.input("(define l5 (make-linear-transform-2d" +
				"  (+ (* 1 2) (* 2 4))" +
				"  (+ (* 1 3) (* 2 5))" +
				"  (+ (* 3 2) (* 4 4))" +
				"  (+ (* 3 3) (* 4 5))))");
		l.input("(define l6 (make-linear-transform-2d 1 0 0 1))");
		l.input("(define l7 (make-linear-transform-2d 1 3 2 4))");
		l.input("(define l8 (make-linear-transform-2d 1 2 3 4))");
		l.input("(define l9 (make-linear-transform-2d 5 6 7 8))");
		l.input("(define la (make-linear-transform-2d" +
				"  (+ (* 1 5) (* 2 7))" +
				"  (+ (* 1 6) (* 2 8))" +
				"  (+ (* 3 5) (* 4 7))" +
				"  (+ (* 3 6) (* 4 8))))");
		eqv(l, "(+ l1 l2)", l.get("l3"));
		eqv(l, "(- l2 l1)", l.get("l4"));
		eqv(l, "(* l1 l2)", l.get("l5"));
		eqv(l, "(matrix-determinant l1)", newR(-2.0));
		eqv(l, "(* (matrix-invert l1) l1)", l.get("l6"));
		eqv(l, "(matrix-transpose l1)", l.get("l7"));
		eqv(l, "(* l8 l9)", l.get("la"));
	}

	public void testLinearRotate() {
		Scheme l = Scheme.newInstance();

		l.input("(define l1 (make-rotation-by-degree-2d 120))");
		l.input("(define l2 (make-rotation-by-degree-2d 90))");
		l.input("(define l3 (make-rotation-by-degree-2d 180))");
		l.input("(define l4 (make-rotation-by-degree-2d 270))");
		l.input("(define lz (make-linear-transform-2d 1 0 0 1))");
//		eqv(l, "(* l1 l1 l1)", l.get("lz"));
//		eqv(l, "(* l2 l2 l2 l2)", l.get("lz"));
//		eqv(l, "(* l3 l3)", l.get("lz"));
//		eqv(l, "(* l4 l2)", l.get("lz"));
//		eqv(l, "(rotate-by-degree-2d l1 240)", l.get("lz"));
//		eqv(l, "(rotate-by-degree-2d l2 270)", l.get("lz"));
//		eqv(l, "(rotate-by-degree-2d l3 180)", l.get("lz"));
//		eqv(l, "(rotate-by-degree-2d l4 90)", l.get("lz"));
	}

	public void testLinearScaleShear() {
		Scheme l = Scheme.newInstance();

		l.input("(define l1 (make-linear-transform-2d 1 2 3 4))");
		l.input("(define l2 (make-linear-transform-2d 2 2 3 12))");
		l.input("(define l3 (make-linear-transform-2d 1 4 9 4))");
		l.input("(define l4 (make-scaling-2d 1 11))");
		l.input("(define l5 (make-shearing-2d 2 3))");
		l.input("(define l6 (make-linear-transform-2d 1 0 0 11))");
		l.input("(define l7 (make-linear-transform-2d 1 2 3 1))");
		l.input("(define l8 (make-linear-transform-2d 4 1 1 4))");
		l.input("(define l9 (make-linear-transform-2d 8 1 1 2))");
		l.input("(define la (make-linear-transform-2d 1 2 3 -4))");
		l.input("(define lb (make-linear-transform-2d -1 2 3 4))");
		eqv(l, "(scale-2d l1 2 3)", l.get("l2"));
		eqv(l, "(shear-2d l1 2 3)", l.get("l3"));
		eqv(l, "l4", l.get("l6"));
		eqv(l, "l5", l.get("l7"));
		eqv(l, "(squeeze-2d l8 2)", l.get("l9"));
		eqv(l, "(reflect-x-2d l1)", l.get("la"));
		eqv(l, "(reflect-y-2d l1)", l.get("lb"));
	}

	public void testAffineOp() {
		Scheme l = Scheme.newInstance();

		// l1       l2
		// (1 2 5)  (5 6 6)
		// (3 4 6)  (7 8 7)
		// (0 0 1)  (0 0 1)
		l.input("(define l1 (make-affine-transform-2d 1 2 3 4 5 6))");
		l.input("(define l2 (make-affine-transform-2d 5 6 7 8 6 7))");
		l.input("(define l5 (make-affine-transform-2d" +
				"  (+ (* 1 5) (* 2 7))" +
				"  (+ (* 1 6) (* 2 8))" +
				"  (+ (* 3 5) (* 4 7))" +
				"  (+ (* 3 6) (* 4 8))" +
				"  (+ (* 1 6) (* 2 7) 5)" +
				"  (+ (* 3 6) (* 4 7) 6)))");
		eqv(l, "(* l1 l2)", l.get("l5"));
		eqv(l, "(matrix-determinant l1)", newR(-2.0));
	}

	public void testAffineRotate() {
		Scheme l = Scheme.newInstance();

		l.input("(define l1 " +
				"  (translate-2d (make-rotation-by-degree-2d 120) 2 3))");
		l.input("(define l2 (make-rotation-by-degree-2d 120))");
		l.input("(define lz (make-affine-transform-2d 1 0 0 1 2 3))");
//		eqv(l, "(* l1 l2 l2)", l.get("lz"));
//		eqv(l, "(rotate-by-degree-2d l1 240)", l.get("lz"));
	}

	public void testAffineScaleShear() {
		Scheme l = Scheme.newInstance();

		l.input("(define l1 (make-affine-transform-2d 1 2 3 4 2 3))");
		l.input("(define l2 (make-affine-transform-2d 2 2 3 12 2 3))");
		l.input("(define l3 (make-affine-transform-2d 1 4 9 4 2 3))");
		l.input("(define l8 (make-affine-transform-2d 4 1 1 4 2 3))");
		l.input("(define l9 (make-affine-transform-2d 8 1 1 2 2 3))");
		l.input("(define la (make-affine-transform-2d 1 2 3 -4 2 3))");
		l.input("(define lb (make-affine-transform-2d -1 2 3 4 2 3))");
		eqv(l, "(scale-2d l1 2 3)", l.get("l2"));
		eqv(l, "(shear-2d l1 2 3)", l.get("l3"));
		eqv(l, "(squeeze-2d l8 2)", l.get("l9"));
		eqv(l, "(reflect-x-2d l1)", l.get("la"));
		eqv(l, "(reflect-y-2d l1)", l.get("lb"));
	}

	public void testConcatenate() {
		Scheme l = Scheme.newInstance();

		// (0  1 0) (1 0 2)
		// (-1 0 0) (0 1 3)
		// (0  0 1) (0 0 1)
		l.input("(define l1 " +
				"  (concatenate-2d" +
				"    (make-rotation-by-degree-2d 90)" +
				"    (make-translation-2d 2 3)))");
		l.input("(define l2 (make-affine-transform-2d 0 1 -1 0 3 -2))");
		l.input("(define l3 (make-linear-transform-2d 1 2 3 4))");
		l.input("(define l4 (make-linear-transform-2d 2 3 4 5))");
		l.input("(define l5 (make-linear-transform-2d" +
				"  (+ (* 1 2) (* 2 4))" +
				"  (+ (* 1 3) (* 2 5))" +
				"  (+ (* 3 2) (* 4 4))" +
				"  (+ (* 3 3) (* 4 5))))");
		l.input("(define l6 (make-affine-transform-2d 1 2 3 4 5 6))");
		l.input("(define l7 (make-affine-transform-2d 2 3 4 5 6 7))");
		l.input("(define l8 (make-affine-transform-2d" +
				"  (+ (* 1 2) (* 2 4))" +
				"  (+ (* 1 3) (* 2 5))" +
				"  (+ (* 3 2) (* 4 4))" +
				"  (+ (* 3 3) (* 4 5))" +
				"  (+ (* 1 6) (* 2 7) 5)" +
				"  (+ (* 3 6) (* 4 7) 6)))");
		eqv(l, "l1", l.get("l2"));
		eqv(l, "(concatenate-2d l3 l4)", l.get("l5"));
		eqv(l, "(concatenate-2d l6 l7)", l.get("l8"));
	}

}
