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

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/21
 */
public class SRFI69Test extends TCSubr {

	public void testHashTableRef() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(define tc (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10))) eqv?))");
		l.exec ("(define td (alist->hash-table" +
				" '((a . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))" +
				" eq? hash-by-identity))");
		equal(l,"(hash-table-ref tb 1)", newZ(2));
		equal(l,"(hash-table-ref tb '(4 5))", list(8, 10));
		lperr(l,"(hash-table-ref tb 6)");
		equal(l,"(hash-table-ref tb 6 (lambda () 0))", newZ(0));
		equal(l,"(hash-table-ref tc 1)", newZ(2));
		lperr(l,"(hash-table-ref tc '(4 5))");
		lperr(l,"(hash-table-ref tc 6)");
		equal(l,"(hash-table-ref td 'a)", newZ(2));
		lperr(l,"(hash-table-ref td '(4 5))");
		lperr(l,"(hash-table-ref td 6)");
	}

	public void testHashTableRefWithDefault() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		equal(l,"(hash-table-ref/default tb 1 0)", newZ(2));
		equal(l,"(hash-table-ref/default tb '(4 5) 0)", list(8, 10));
		equal(l,"(hash-table-ref/default tb 6 0)", newZ(0));
	}

	public void testHashTableSetS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (make-hash-table))");
		l.exec ("(hash-table-set! tb 1 2)");
		equal(l,"(hash-table-ref tb 1)", newZ(2));
	}

	public void testHashTableDeleteS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(hash-table-delete! tb 1)");
		equal(l,"(hash-table-ref/default tb 1 0)", newZ(0));
	}

	public void testIsHashTableExists() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		equal(l,"(hash-table-exists? tb 1)", T);
		equal(l,"(hash-table-exists? tb '(4 5))", T);
		equal(l,"(hash-table-exists? tb 6)", F);
	}

	public void testHashTableUpdateS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(define (^3 x) (* x x x))");
		equal(l,"(hash-table-update! tb 1 ^3)", newZ(8));
		lperr(l,"(hash-table-update! tb 6 ^3)");
		equal(l,"(hash-table-update! tb 6 ^3 (lambda () 0))",
				newZ(0));
	}

	public void testHashTableUpdateSWithDefault() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(define (^3 x) (* x x x))");
		equal(l,"(hash-table-update!/default tb 1 ^3 0)", newZ(8));
		equal(l,"(hash-table-update!/default tb 6 ^3 0)", newZ(0));
	}

	public void testHashTableSize() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		equal(l,"(hash-table-size tb)", newZ(4));
	}

	public void testHashTableKeys() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(display (hash-table-keys tb))");
	}

	public void testHashTableValues() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(display (hash-table-values tb))");
	}

	public void testHashTableWalk() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(hash-table-walk tb" +
				" (lambda (x y) (format #t \"~a ~a\" x y)))");
	}

	public void testHashTableFold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6))))");
		equal(l,"(hash-table-fold tb (lambda (x y z) (+ x y z)) 0)",
				newZ(18));
	}

	public void testHashTableToAlist() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		equal(l,"(alist-map= equal?" +
				" (hash-table->alist tb)" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10))))", T);
	}

	public void testHashTableCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(define tc (hash-table-copy tb))");
		equal(l,"(alist-map= equal?" +
				" (hash-table->alist tb)" +
				" (hash-table->alist tc))", T);
		l.exec ("(hash-table-set! tb 1 3)");
		equal(l,"(alist-map= equal?" +
				" (hash-table->alist tb)" +
				" (hash-table->alist tc))", F);
	}

	public void testHashTableMergeS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define tb (alist->hash-table" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10)))))");
		l.exec ("(define tc (alist->hash-table" +
				" '((6 . 12) (8 . 16))))");
		l.exec ("(hash-table-merge! tb tc)");
		equal(l,"(alist-map= equal?" +
				" (hash-table->alist tb)" +
				" '((1 . 2) (2 . 4) (3 . 6) ((4 5) . (8 10))" +
				"   (6 . 12) (8 . 16)))", T);
	}

	public void testHash() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (hash 'a) (hash 'a))", T);
		equal(l,"(eqv? (hash 1) (hash 1))", T);
		equal(l,"(eqv? (hash '(1 2)) (hash '(1 2)))", T);
		equal(l,"(eqv? (hash '#(1 2)) (hash '#(1 2)))", T);
		equal(l,"(eqv? (hash \"12\") (hash \"12\"))", T);
	}

	public void testStringHash() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (string-hash \"abc\") (string-hash \"abc\"))", T);
	}

	public void testStringCiHash() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (string-ci-hash \"abc\") (string-ci-hash \"abc\"))", T);
		equal(l,"(eqv? (string-ci-hash \"abc\") (string-ci-hash \"AbC\"))", T);
	}

	public void testHashByIdentity() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (hash-by-identity 'a) (hash-by-identity 'a))", T);
	}

}
