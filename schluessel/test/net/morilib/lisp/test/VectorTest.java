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
import net.morilib.lisp.Nil;

public class VectorTest extends TCSubr {
	
	public void testIsVector() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(vector? #f)", F);
		eq(l, "(vector? 'a)", F);
		eq(l, "(vector? #\\a)", F);
		eq(l, "(vector? #(1 2))", T);
		eq(l, "(vector? vector?)", F);
		eq(l, "(vector? '(1 2))", F);
		eq(l, "(vector? 1)", F);
		eq(l, "(vector? \"aaa\")", F);
		eq(l, "(vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}
	
	public void testVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(vector 'a 2 #\\c)", vec(sym("a"), 2, 'c'));
		equal(l, "(vector '(1 2) 3)", vec(list(1, 2), 3));
		equal(l, "(vector)", vec());
	}
	
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-vector 4 1)", vec(1, 1, 1, 1));
		equal(l, "(make-vector 0 1)", vec());
		equal(l, "(make-vector 0)", vec());
		lperr(l, "(make-vector 4.0)");
		lperr(l, "(make-vector)");
	}
	
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(vector-length '#(1 2 3 4))", 4);
		eqi(l, "(vector-length '#())", 0);
		lperr(l, "(vector-length '(1 2 3 4))");
		lperr(l, "(vector-length '())");
		lperr(l, "(vector-legnth)");
	}
	
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(vector-ref '#(1 2 3 4) 2)", 3);
		eqi(l, "(vector-ref '#(1 2 3 4) 0)", 1);
		eqi(l, "(vector-ref '#(1 2 3 4) 3)", 4);
		lperr(l, "(vector-ref '#(1 2 3 4) 4)");
		lperr(l, "(vector-ref '#(1 2 3 4) -1)");
		lperr(l, "(vector-ref '(1 2 3 4) 1)");
		lperr(l, "(vector-ref '#(1 2 3 4) 1.0)");
		lperr(l, "(vector-ref '#(1 2 3 4))");
	}
	
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(vector-set! v 1 #\\b)");
		equal(l.get("v"), vec(1, 'b', 3, 4));
		lperr(l, "(vector-set! v -1 #\\c)");
		lperr(l, "(vector-set! v 4 #\\z)");
		lperr(l, "(vector-set! v 1.0 #\\z)");
		lperr(l, "(vector-set! l 1 #\\z)");
		lperr(l, "(vector-set! v 1)");
	}
	
	public void testVectorFillS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(vector-fill! v 9)");
		equal(l.get("v"), vec(9, 9, 9, 9));
		lperr(l, "(vector-fill! l 'a)");
		lperr(l, "(vector-fill! v)");
	}
	
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(vector->list '#(1 2 3 4))", list(1, 2, 3, 4));
		equal(l, "(vector->list '#())", Nil.NIL);
		lperr(l, "(vector->list 1)");
		lperr(l, "(vector->list '(1 2 3 4))");
		lperr(l, "(vector->list)");
	}
	
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->vector '(1 2 3 4))", vec(1, 2, 3, 4));
		equal(l, "(list->vector '())", vec());
		lperr(l, "(list->vector 1)");
		lperr(l, "(list->vector '#(1 2 3 4))");
		lperr(l, "(list->vector '(1 2 3 4 . 5))");
		lperr(l, "(list->vector)");
	}
	
}
