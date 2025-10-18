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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispFloat;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.array.LispArrayC32;
import net.morilib.lisp.array.LispArrayC64;
import net.morilib.lisp.array.LispArrayR32;
import net.morilib.lisp.array.LispArrayR64;
import net.morilib.lisp.array.LispArrayS16;
import net.morilib.lisp.array.LispArrayS32;
import net.morilib.lisp.array.LispArrayS64;
import net.morilib.lisp.array.LispArrayS8;
import net.morilib.lisp.array.LispArrayU16;
import net.morilib.lisp.array.LispArrayU32;
import net.morilib.lisp.array.LispArrayU64;
import net.morilib.lisp.array.LispArrayU8;
import net.morilib.lisp.array.LispCharArray;
import net.morilib.lisp.array.LispDefaultArray;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/26
 */
public class SRFI47Test extends TCSubr {

	public void testEquals() {
		Scheme l = Scheme.newInstance();

		l.exec("(define a1 (make-array (ac64 1) 2 2 2))");
		l.exec("(define a2 (make-array (ac32 1) 2 2 2))");
		l.exec("(define a3 (make-array (ar64 1) 2 2 2))");
		l.exec("(define a4 (make-array (ar32 1) 2 2 2))");
		l.exec("(define a5 (make-array (as64 1) 2 2 2))");
		l.exec("(define a6 (make-array (as32 1) 2 2 2))");
		l.exec("(define a7 (make-array (as16 1) 2 2 2))");
		l.exec("(define a8 (make-array (as8  1) 2 2 2))");
		l.exec("(define a9 (make-array (au64 1) 2 2 2))");
		l.exec("(define aa (make-array (au32 1) 2 2 2))");
		l.exec("(define ab (make-array (au16 1) 2 2 2))");
		l.exec("(define ac (make-array (au8  1) 2 2 2))");
		l.exec("(define ad (make-array '#(1) 2 2 2))");
		l.exec("(define b1 (make-array (ac64 1) 2 2 2))");
		l.exec("(define b2 (make-array (ac32 1) 2 2 2))");
		l.exec("(define b3 (make-array (ar64 1) 2 2 2))");
		l.exec("(define b4 (make-array (ar32 1) 2 2 2))");
		l.exec("(define b5 (make-array (as64 1) 2 2 2))");
		l.exec("(define b6 (make-array (as32 1) 2 2 2))");
		l.exec("(define b7 (make-array (as16 1) 2 2 2))");
		l.exec("(define b8 (make-array (as8  1) 2 2 2))");
		l.exec("(define b9 (make-array (au64 1) 2 2 2))");
		l.exec("(define ba (make-array (au32 1) 2 2 2))");
		l.exec("(define bb (make-array (au16 1) 2 2 2))");
		l.exec("(define bc (make-array (au8  1) 2 2 2))");
		l.exec("(define bd (make-array '#(1) 2 2 2))");
//		l.exec("(define be (make-array '#(1.0) 2 2 2))");
		l.exec("(define c1 (make-array (ac64 2) 2 2 2))");
		l.exec("(define c2 (make-array (ac32 2) 2 2 2))");
		l.exec("(define c3 (make-array (ar64 2) 2 2 2))");
		l.exec("(define c4 (make-array (ar32 2) 2 2 2))");
		l.exec("(define c5 (make-array (as64 2) 2 2 2))");
		l.exec("(define c6 (make-array (as32 2) 2 2 2))");
		l.exec("(define c7 (make-array (as16 2) 2 2 2))");
		l.exec("(define c8 (make-array (as8  2) 2 2 2))");
		l.exec("(define c9 (make-array (au64 2) 2 2 2))");
		l.exec("(define ca (make-array (au32 2) 2 2 2))");
		l.exec("(define cb (make-array (au16 2) 2 2 2))");
		l.exec("(define cc (make-array (au8  2) 2 2 2))");
		l.exec("(define cd (make-array '#(2) 2 2 2))");
		l.exec("(define d1 (make-array (ac64 1) 2 2 1))");
		l.exec("(define d2 (make-array (ac32 1) 2 2 1))");
		l.exec("(define d3 (make-array (ar64 1) 2 2 1))");
		l.exec("(define d4 (make-array (ar32 1) 2 2 1))");
		l.exec("(define d5 (make-array (as64 1) 2 2 1))");
		l.exec("(define d6 (make-array (as32 1) 2 2 1))");
		l.exec("(define d7 (make-array (as16 1) 2 2 1))");
		l.exec("(define d8 (make-array (as8  1) 2 2 1))");
		l.exec("(define d9 (make-array (au64 1) 2 2 1))");
		l.exec("(define da (make-array (au32 1) 2 2 1))");
		l.exec("(define db (make-array (au16 1) 2 2 1))");
		l.exec("(define dc (make-array (au8  1) 2 2 1))");
		l.exec("(define dd (make-array '#(1) 2 2 1))");
		l.exec("(define e1 (make-array (ac64 1) 2 2))");
		l.exec("(define e2 (make-array (ac32 1) 2 2))");
		l.exec("(define e3 (make-array (ar64 1) 2 2))");
		l.exec("(define e4 (make-array (ar32 1) 2 2))");
		l.exec("(define e5 (make-array (as64 1) 2 2))");
		l.exec("(define e6 (make-array (as32 1) 2 2))");
		l.exec("(define e7 (make-array (as16 1) 2 2))");
		l.exec("(define e8 (make-array (as8  1) 2 2))");
		l.exec("(define e9 (make-array (au64 1) 2 2))");
		l.exec("(define ea (make-array (au32 1) 2 2))");
		l.exec("(define eb (make-array (au16 1) 2 2))");
		l.exec("(define ec (make-array (au8  1) 2 2))");
		l.exec("(define ed (make-array '#(1) 2 2))");
		l.exec("(define at (make-array (at1 1) 2 2 3))");
		l.exec("(define bt (make-array (at1 #f) 2 2 3))");
		l.exec("(array-set! bt 0 0 0 #t)");
		l.exec("(array-set! bt 0 0 1 #t)");
		l.exec("(array-set! bt 0 0 2 #t)");
		l.exec("(array-set! bt 0 1 0 #t)");
		l.exec("(array-set! bt 0 1 1 #t)");
		l.exec("(array-set! bt 0 1 2 #t)");
		l.exec("(array-set! bt 1 0 0 #t)");
		l.exec("(array-set! bt 1 0 1 #t)");
		l.exec("(array-set! bt 1 0 2 #t)");
		l.exec("(array-set! bt 1 1 0 #t)");
		l.exec("(array-set! bt 1 1 1 #t)");
//		l.exec("(array-set! bt 1 1 2 #t)");
		l.exec("(define ct (make-array '#(#t) 2 2 3))");

		equal(l,"(equal? a1 b1)", T);
		equal(l,"(equal? a2 b2)", T);
		equal(l,"(equal? a3 b3)", T);
		equal(l,"(equal? a4 b4)", T);
		equal(l,"(equal? a5 b5)", T);
		equal(l,"(equal? a6 b6)", T);
		equal(l,"(equal? a7 b7)", T);
		equal(l,"(equal? a8 b8)", T);
		equal(l,"(equal? a9 b9)", T);
		equal(l,"(equal? aa ba)", T);
		equal(l,"(equal? ab bb)", T);
		equal(l,"(equal? ac bc)", T);
		equal(l,"(equal? ad bd)", T);
//
//		equal(l,"(equal? a1 be)", T);
//		equal(l,"(equal? a2 be)", T);
//		equal(l,"(equal? a3 be)", T);
//		equal(l,"(equal? a4 be)", T);
		equal(l,"(equal? a5 bd)", T);
		equal(l,"(equal? a6 bd)", T);
		equal(l,"(equal? a7 bd)", T);
		equal(l,"(equal? a8 bd)", T);
		equal(l,"(equal? a9 bd)", T);
		equal(l,"(equal? aa bd)", T);
		equal(l,"(equal? ab bd)", T);
		equal(l,"(equal? ac bd)", T);

		equal(l,"(equal? a1 c1)", F);
		equal(l,"(equal? a2 c2)", F);
		equal(l,"(equal? a3 c3)", F);
		equal(l,"(equal? a4 c4)", F);
		equal(l,"(equal? a5 c5)", F);
		equal(l,"(equal? a6 c6)", F);
		equal(l,"(equal? a7 c7)", F);
		equal(l,"(equal? a8 c8)", F);
		equal(l,"(equal? a9 c9)", F);
		equal(l,"(equal? aa ca)", F);
		equal(l,"(equal? ab cb)", F);
		equal(l,"(equal? ac cc)", F);
		equal(l,"(equal? ad cd)", F);

		equal(l,"(equal? a1 cd)", F);
		equal(l,"(equal? a2 cd)", F);
		equal(l,"(equal? a3 cd)", F);
		equal(l,"(equal? a4 cd)", F);
		equal(l,"(equal? a5 cd)", F);
		equal(l,"(equal? a6 cd)", F);
		equal(l,"(equal? a7 cd)", F);
		equal(l,"(equal? a8 cd)", F);
		equal(l,"(equal? a9 cd)", F);
		equal(l,"(equal? aa cd)", F);
		equal(l,"(equal? ab cd)", F);
		equal(l,"(equal? ac cd)", F);

		equal(l,"(equal? a1 d1)", F);
		equal(l,"(equal? a2 d2)", F);
		equal(l,"(equal? a3 d3)", F);
		equal(l,"(equal? a4 d4)", F);
		equal(l,"(equal? a5 d5)", F);
		equal(l,"(equal? a6 d6)", F);
		equal(l,"(equal? a7 d7)", F);
		equal(l,"(equal? a8 d8)", F);
		equal(l,"(equal? a9 d9)", F);
		equal(l,"(equal? aa da)", F);
		equal(l,"(equal? ab db)", F);
		equal(l,"(equal? ac dc)", F);
		equal(l,"(equal? ad dd)", F);

		equal(l,"(equal? a1 e1)", F);
		equal(l,"(equal? a2 e2)", F);
		equal(l,"(equal? a3 e3)", F);
		equal(l,"(equal? a4 e4)", F);
		equal(l,"(equal? a5 e5)", F);
		equal(l,"(equal? a6 e6)", F);
		equal(l,"(equal? a7 e7)", F);
		equal(l,"(equal? a8 e8)", F);
		equal(l,"(equal? a9 e9)", F);
		equal(l,"(equal? aa ea)", F);
		equal(l,"(equal? ab eb)", F);
		equal(l,"(equal? ac ec)", F);
		equal(l,"(equal? ad ed)", F);

		equal(l,"(equal? at bt)", F);
		l.exec("(array-set! bt 1 1 2 #t)");
		equal(l,"(equal? at bt)", T);
		equal(l,"(equal? at ct)", T);
	}

	private static LispComplex newCR(int r, int i) {
		return LispComplex.newComplex(newR((double)r), newR((double)i));
	}

	private static LispComplex newCF(int r, int i) {
		return LispComplex.newComplex(new LispFloat(r),
				new LispFloat(i));
	}

	public void testMakeArray() {
		Scheme l = Scheme.newInstance();

		l.exec("(define a (make-array (ac64 1+i) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayC64);
		equal(l,"(array-ref a 0 0 0)", newCR(1, 1));
		equal(l,"(array-ref a 0 0 1)", newCR(1, 1));
		equal(l,"(array-ref a 0 1 0)", newCR(1, 1));
		equal(l,"(array-ref a 0 1 1)", newCR(1, 1));
		equal(l,"(array-ref a 1 0 0)", newCR(1, 1));
		equal(l,"(array-ref a 1 0 1)", newCR(1, 1));
		equal(l,"(array-ref a 1 1 0)", newCR(1, 1));
		equal(l,"(array-ref a 1 1 1)", newCR(1, 1));

		l.exec("(define a (make-array (ac32 1+i) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayC32);
		equal(l,"(array-ref a 0 0 0)", newCF(1, 1));
		equal(l,"(array-ref a 0 0 1)", newCF(1, 1));
		equal(l,"(array-ref a 0 1 0)", newCF(1, 1));
		equal(l,"(array-ref a 0 1 1)", newCF(1, 1));
		equal(l,"(array-ref a 1 0 0)", newCF(1, 1));
		equal(l,"(array-ref a 1 0 1)", newCF(1, 1));
		equal(l,"(array-ref a 1 1 0)", newCF(1, 1));
		equal(l,"(array-ref a 1 1 1)", newCF(1, 1));

		l.exec("(define a (make-array (ar64 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayR64);
		equal(l,"(array-ref a 0 0 0)", newR(1.0));
		equal(l,"(array-ref a 0 0 1)", newR(1.0));
		equal(l,"(array-ref a 0 1 0)", newR(1.0));
		equal(l,"(array-ref a 0 1 1)", newR(1.0));
		equal(l,"(array-ref a 1 0 0)", newR(1.0));
		equal(l,"(array-ref a 1 0 1)", newR(1.0));
		equal(l,"(array-ref a 1 1 0)", newR(1.0));
		equal(l,"(array-ref a 1 1 1)", newR(1.0));

		l.exec("(define a (make-array (ar32 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayR32);
		equal(l,"(array-ref a 0 0 0)", new LispFloat(1));
		equal(l,"(array-ref a 0 0 1)", new LispFloat(1));
		equal(l,"(array-ref a 0 1 0)", new LispFloat(1));
		equal(l,"(array-ref a 0 1 1)", new LispFloat(1));
		equal(l,"(array-ref a 1 0 0)", new LispFloat(1));
		equal(l,"(array-ref a 1 0 1)", new LispFloat(1));
		equal(l,"(array-ref a 1 1 0)", new LispFloat(1));
		equal(l,"(array-ref a 1 1 1)", new LispFloat(1));

		l.exec("(define a (make-array (as64 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayS64);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (as32 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayS32);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (as16 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayS16);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (as8  1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayS8);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (au64 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayU64);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (au32 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayU32);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (au16 1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayU16);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array (au8  1) 2 2 2))");
		ok(l.exec("a") instanceof LispArrayU8);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));

		l.exec("(define a (make-array \"1\" 2 2 2))");
		ok(l.exec("a") instanceof LispCharArray);
		equal(l,"(array-ref a 0 0 0)", chr('1'));
		equal(l,"(array-ref a 0 0 1)", chr('1'));
		equal(l,"(array-ref a 0 1 0)", chr('1'));
		equal(l,"(array-ref a 0 1 1)", chr('1'));
		equal(l,"(array-ref a 1 0 0)", chr('1'));
		equal(l,"(array-ref a 1 0 1)", chr('1'));
		equal(l,"(array-ref a 1 1 0)", chr('1'));
		equal(l,"(array-ref a 1 1 1)", chr('1'));

		l.exec("(define a (make-array '#(1) 2 2 2))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(1));
		equal(l,"(array-ref a 0 1 0)", newZ(1));
		equal(l,"(array-ref a 0 1 1)", newZ(1));
		equal(l,"(array-ref a 1 0 0)", newZ(1));
		equal(l,"(array-ref a 1 0 1)", newZ(1));
		equal(l,"(array-ref a 1 1 0)", newZ(1));
		equal(l,"(array-ref a 1 1 1)", newZ(1));
	}

	public void testMakeSharedArray() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (make-array (ac64 1+i) 2 2 2))");
		l.exec ("(define b (make-shared-array a" +
				"  (lambda (x) (list x x x)) 2))");
		l.exec ("(array-set! b 0 1+2i)");
		l.exec ("(array-set! b 1 1+3i)");
		equal(l,"(array-ref a 0 0 0)", newCR(1, 2));
		equal(l,"(array-ref a 0 0 1)", newCR(1, 1));
		equal(l,"(array-ref a 0 1 0)", newCR(1, 1));
		equal(l,"(array-ref a 0 1 1)", newCR(1, 1));
		equal(l,"(array-ref a 1 0 0)", newCR(1, 1));
		equal(l,"(array-ref a 1 0 1)", newCR(1, 1));
		equal(l,"(array-ref a 1 1 0)", newCR(1, 1));
		equal(l,"(array-ref a 1 1 1)", newCR(1, 3));
		equal(l,"(array-ref b 0)", newCR(1, 2));
		equal(l,"(array-ref b 1)", newCR(1, 3));
	}

	public void testArrayRank() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(array-rank (make-array (ac64 1) 2 2 2))", 3);
	}

	public void testArrayDimensions() {
		Scheme l = Scheme.newInstance();

		equal(l,"(array-dimensions (make-array (au8 1) 2 3 4))",
				list(2, 3, 4));
	}

	public void testIsArrayInBounds() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (make-array (ac64 1+i) 2 2 2))");
		equal(l,"(array-in-bounds? a 0 0 0)", T);
		equal(l,"(array-in-bounds? a 0 0 3)", F);
		equal(l,"(array-in-bounds? a 0 0)", F);
		equal(l,"(array-in-bounds? a 0 0 0 0)", F);
	}

	public void testArrayRef() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (make-array (ac64 1+2i) 2 2 2))");
		equal(l,"(array-ref a 0 0 0)", newCR(1, 2));
		lperr(l,"(array-ref a 0 0 2)");
		lperr(l,"(array-ref a 0 0)");
		lperr(l,"(array-ref a 0 0 0 0)");
	}

	public void testArraySetS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a1 (make-array (ac64 1) 2 2 2))");
		l.exec ("(define a2 (make-array (ac32 1) 2 2 2))");
		l.exec ("(define a3 (make-array (ar64 1) 2 2 2))");
		l.exec ("(define a4 (make-array (ar32 1) 2 2 2))");
		l.exec ("(define a5 (make-array (as64 1) 2 2 2))");
		l.exec ("(define a6 (make-array (as32 1) 2 2 2))");
		l.exec ("(define a7 (make-array (as16 1) 2 2 2))");
		l.exec ("(define a8 (make-array (as8  1) 2 2 2))");
		l.exec ("(define a9 (make-array (au64 1) 2 2 2))");
		l.exec ("(define aa (make-array (au32 1) 2 2 2))");
		l.exec ("(define ab (make-array (au16 1) 2 2 2))");
		l.exec ("(define ac (make-array (au8  1) 2 2 2))");
		l.exec ("(array-set! a1 0 0 0 2)");
		l.exec ("(array-set! a2 0 0 0 2)");
		l.exec ("(array-set! a3 0 0 0 2)");
		l.exec ("(array-set! a4 0 0 0 2)");
		l.exec ("(array-set! a5 0 0 0 2)");
		l.exec ("(array-set! a6 0 0 0 2)");
		l.exec ("(array-set! a7 0 0 0 2)");
		l.exec ("(array-set! a8 0 0 0 2)");
		l.exec ("(array-set! a9 0 0 0 2)");
		l.exec ("(array-set! aa 0 0 0 2)");
		l.exec ("(array-set! ab 0 0 0 2)");
		l.exec ("(array-set! ac 0 0 0 2)");
		equal(l,"(array-ref a1 0 0 0)", newC(2.0, 0.0));
		equal(l,"(array-ref a2 0 0 0)", newC(2.0f, 0.0f));
		equal(l,"(array-ref a3 0 0 0)", newR(2.0));
		equal(l,"(array-ref a4 0 0 0)", newR(2.0f));
		equal(l,"(array-ref a5 0 0 0)", newZ(2));
		equal(l,"(array-ref a6 0 0 0)", newZ(2));
		equal(l,"(array-ref a7 0 0 0)", newZ(2));
		equal(l,"(array-ref a8 0 0 0)", newZ(2));
		equal(l,"(array-ref a9 0 0 0)", newZ(2));
		equal(l,"(array-ref aa 0 0 0)", newZ(2));
		equal(l,"(array-ref ab 0 0 0)", newZ(2));
		equal(l,"(array-ref ac 0 0 0)", newZ(2));
		lperr(l,"(array-ref a1 0 0 2 2)");
		lperr(l,"(array-ref a1 0 0 2)");
		lperr(l,"(array-ref a1 0 0 0 0 2)");

		lperr(l,"(array-set! a5 0 0 0 #x8000000000000000)");
		lperr(l,"(array-set! a6 0 0 0 #x80000000)");
		lperr(l,"(array-set! a7 0 0 0 #x8000)");
		lperr(l,"(array-set! a8 0 0 0 #x80)");
		lperr(l,"(array-set! a9 0 0 0 #x10000000000000000)");
		lperr(l,"(array-set! aa 0 0 0 #x100000000)");
		lperr(l,"(array-set! ab 0 0 0 #x10000)");
		lperr(l,"(array-set! ac 0 0 0 #x100)");

		lperr(l,"(array-set! a5 0 0 0 -9223372036854775809)");
		lperr(l,"(array-set! a6 0 0 0 −2147483649)");
		lperr(l,"(array-set! a7 0 0 0 -32769)");
		lperr(l,"(array-set! a8 0 0 0 -129)");
		lperr(l,"(array-set! a9 0 0 0 -1)");
		lperr(l,"(array-set! aa 0 0 0 -1)");
		lperr(l,"(array-set! ab 0 0 0 -1)");
		lperr(l,"(array-set! ac 0 0 0 -1)");
	}

}
