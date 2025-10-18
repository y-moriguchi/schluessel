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
 * @author MORIGUCHI, Yuichiro 2011/06/05
 */
public class SRFI41Test extends TCSubr {

	public void testIsStream() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream? stream-null)", T);
		equal(l,"(stream? (stream 1 2 3))", T);
		equal(l,"(stream? (list 1 2 3))", F);
	}

	public void testIsStreamNull() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-null? stream-null)", T);
		equal(l,"(stream-null? (stream 1 2))", F);
	}

	public void testIsStreamPair() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-pair? stream-null)", F);
		equal(l,"(stream-pair? (stream 1 2))", T);
	}

	public void testDefineStream() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		l.exec ("(define-stream (aaa b . c)" +
				"  (display (list b . c))" +
				"  stream-null)");
		l.exec ("(aaa 1 2 3)");
	}

	public void testListToStream() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		l.exec ("(define s (list->stream '(1)))");
		equal(l,"(stream-car s)", newZ(1));
		equal(l,"(stream-null? (stream-cdr s))", T);
	}

	public void testPortToStream() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (port->stream" +
				"  (open-input-string \"abc\")))",
				list('a', 'b', 'c'));
	}

	public void testStream() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		l.exec ("(define s (stream 1 (/ 1 0) -1))");
	}

	public void testStreamToList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 10" +
				"  (stream-map (lambda (x) (* x x))" +
				"  (stream-from 0)))",
				list(0, 1, 4, 9, 16, 25, 36, 49, 64, 81));
		lperr(l,("(stream->list #t stream-null)"));
	}

	public void testStreamAppend() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-append " +
				"  (stream 1 2) (stream 3 4) (stream 5)))",
				list(1, 2, 3, 4, 5));
		equal(l,"(stream->list (stream-append))", list());
	}

	public void testStreamConcat() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-concat " +
				"  (stream (stream 1 2) (stream 3 4) (stream 5))))",
				list(1, 2, 3, 4, 5));
		equal(l,"(stream->list (stream-concat stream-null))", list());
	}

	public void testStreamConstant() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 4 (stream-constant 1))",
				list(1, 1, 1, 1));
		equal(l,"(stream->list 5 (stream-constant 1 2))",
				list(1, 2, 1, 2, 1));
	}

	public void testStreamDrop() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-drop 3 " +
				"  (stream 1 2 3 4 5)))",
				list(4, 5));
		equal(l,"(stream->list (stream-drop 3 (stream 1 2 3)))",
				list());
		equal(l,"(stream->list (stream-drop 3 (stream 1 2)))",
				list());
		lperr(l,"(stream-drop #t stream-null)");
	}

	public void testStreamDropWhile() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-drop-while integer?" +
				"  (stream 1 2 3 4.4 5.5)))",
				list(4.4, 5.5));
		equal(l,"(stream->list (stream-drop-while integer?" +
				"  (stream 1 2 3 4 5)))",
				list());
	}

	public void testStreamFilter() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 5 (stream-filter odd? (stream-from 0)))",
				list(1, 3, 5, 7, 9));
	}

	public void testStreamFold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-fold + 0 (stream 1 2 3 4 5))", newZ(15));
	}

	public void testStreamForEach() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		l.exec ("(stream-for-each (lambda x (display (apply + x)))" +
				"  (stream 1 2 3) (stream 4 5 9) (stream 6 7 8))");
		l.exec ("(stream-for-each (lambda x (display (apply + x)))" +
				"  (stream 1 2 3) (stream) (stream 6 7 8))");
	}

	public void testStreamFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 5 (stream-from 0))",
				list(0, 1, 2, 3, 4));
		equal(l,"(stream->list 5 (stream-from 1 2))",
				list(1, 3, 5, 7, 9));
		lperr(l,"(stream-from #t)");
		lperr(l,"(stream-from 1 #t)");
		lperr(l,"(stream-from 1 2 3)");
	}

	public void testStreamIterate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 5 (stream-iterate (lambda (x) (+ x 1)) 0))",
				list(0, 1, 2, 3, 4));
		equal(l,"(stream->list 5 (stream-iterate (lambda (x) (* x 2)) 1))",
				list(1, 2, 4, 8, 16));
	}

	public void testStreamLength() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-length (stream 1 2 3 4 5))", newZ(5));
		equal(l,"(stream-length (stream))", newZ(0));
		equal(l,"(stream-length stream-null)", newZ(0));
	}

	public void testStreamMap() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-map (lambda x (+ . x))" +
				"  (stream 1 2 3) (stream 4 5) (stream 6 7 8)))",
				list(11, 14));
		equal(l,"(stream->list (stream-map (lambda x (+ . x))" +
				"  (stream 1 2 3) (stream) (stream 6 7 8)))",
				list());
	}

	public void testStreamMatch() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-match (stream 1 2 3)" +
				"  (() (error \"aaa\"))" +
				"  ((x . y) (list (stream->list y) x)))",
				list(list(2, 3), 1));
		equal(l,"(stream-match (stream 1 2 3)" +
				"  (() (error \"aaa\"))" +
				"  ((2 x y) (error \"bbb\"))" +
				"  ((1 x y) (list y x))" +
				"  ((x y z) (error \"ccc\")))", list(3, 2));
		equal(l,"(stream-match (stream 1 (stream 2 3))" +
				"  (() (error \"aaa\"))" +
				"  ((1 x y) (error \"bbb\"))" +
				"  ((1 (x y)) (list y x))" +
				"  ((x y) (error \"ccc\")))", list(3, 2));
		equal(l,"(stream-match (stream 1 2 2)" +
				"  (() (error \"aaa\"))" +
				"  ((2 x y) (error \"bbb\"))" +
				"  ((1 x y) (= x y) (list y x))" +
				"  ((x y z) (error \"ccc\")))", list(2, 2));
		equal(l,"(stream-match (stream 1 2 3)" +
				"  (() (error \"aaa\"))" +
				"  ((2 x y) (error \"bbb\"))" +
				"  ((1 x y) (= x y) (error \"ccc\"))" +
				"  ((x y z) (list x y z)))", list(1, 2, 3));
	}

	public void testStreamOf() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-of (* x x)" +
				"  (x in (stream-range 0 10))" +
				"  (y is x)" +
				"  (even? y)))", list(0, 4, 16, 36, 64));
	}

	public void testStreamRange() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-range 0 10))",
				list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
		equal(l,"(stream->list (stream-range 0 10 2))",
				list(0, 2, 4, 6, 8));
		lperr(l,"(stream-range #t 10)");
		lperr(l,"(stream-range 1 #t)");
		lperr(l,"(stream-range 1 10 #t)");
	}

	public void testStreamRef() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream-ref (stream 1 2 3 4) 1)", newZ(2));
		lperr(l,"(stream-ref (stream) 0)");
		lperr(l,"(stream-ref (stream 1 2 3) -1)");
		lperr(l,"(stream-ref (stream 1 2 3) 3)");
	}

	public void testStreamReverse() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-reverse (stream 1 2 3 4)))",
				list(4, 3, 2, 1));
		equal(l,"(stream->list (stream-reverse (stream)))",
				list());
	}

	public void testStreamScan() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list 6 (stream-scan + 0 (stream-from 1)))",
				list(0, 1, 3, 6, 10, 15));
	}

	public void testStreamTake() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-take 3 " +
				"  (stream 1 2 3 4 5)))",
				list(1, 2, 3));
		equal(l,"(stream->list (stream-take 3 (stream 1 2 3)))",
				list(1, 2, 3));
		equal(l,"(stream->list (stream-take 3 (stream 1 2)))",
				list(1, 2));
		lperr(l,"(stream-drop #t stream-null)");
	}

	public void testStreamTakeWhile() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-take-while integer?" +
				"  (stream 1 2 3 4.4 5.5)))",
				list(1, 2, 3));
		equal(l,"(stream->list (stream-take-while integer?" +
				"  (stream 1 2 3 4 5)))",
				list(1, 2, 3, 4, 5));
	}

	public void testStreamUnfold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-unfold" +
				"  (lambda (x) (expt x 2))" +
				"  (lambda (x) (< x 10))" +
				"  (lambda (x) (+ x 1))" +
				"  0))", list(0, 1, 4, 9, 16, 25, 36, 49, 64, 81));
	}

	public void testStreamUnfolds() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		l.exec ("(define (stream-partition pred? strm)" +
				"  (stream-unfolds" +
				"    (lambda (s)" +
				"      (if (stream-null? s)" +
				"          (values s '() '())" +
				"          (let ((a (stream-car s))" +
				"                (d (stream-cdr s)))" +
				"            (if (pred? a)" +
				"                (values d (list a) #f)" +
				"                (values d #f (list a))))))" +
				"    strm))");
		equal(l,"(receive (r1 r2)" +
				"         (stream-partition odd? (stream-range 1 6))" +
				"  (list (stream->list r1) (stream->list r2)))",
				list(list(1, 3, 5), list(2, 4)));
	}

	public void testStreamZip() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-41)");
		equal(l,"(stream->list (stream-zip" +
				"  (stream 1 2 3) (stream 4 5) (stream 6 7 8)))",
				list(list(1, 4, 6), list(2, 5, 7)));
	}

}
