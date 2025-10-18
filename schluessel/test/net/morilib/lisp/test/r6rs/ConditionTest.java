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
 * @author MORIGUCHI, Yuichiro 2011/09/11
 */
public class ConditionTest extends TCSubr {

	public void testCondition() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define d (condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		equal(l,"(condition-has-type? d &test)", T);
		equal(l,"(condition-has-type? d &test2)", T);
		equal(l,"(condition-has-type? d &condition)", T);
		equal(l,"(condition-has-type? d &error)", F);
		eqi  (l,"(condition-ref d 'aaa)", 1);
		eqi  (l,"(condition-ref d 'ccc)", 2);
	}

	public void testSimpleConditions() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define d (condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		l.exec ("(define l (simple-conditions d))");
		equal(l,"(condition-has-type? (car  l) &test)", T);
		equal(l,"(condition-has-type? (cadr l) &test2)", T);
		equal(l,"(null? (cddr l))", T);
		l.exec ("(define d (make-condition &test 'aaa 1))");
		l.exec ("(define l (simple-conditions d))");
		equal(l,"(condition-has-type? (car  l) &test)", T);
		equal(l,"(null? (cdr l))", T);
	}

	public void testIsConditions() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define d (condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		equal(l,"(condition? d)", T);
		l.exec ("(define d (make-condition &test 'aaa 1))");
		equal(l,"(condition? d)", T);
		equal(l,"(condition? &test)", F);
		equal(l,"(condition? 1)", F);
	}

	public void testConditionPredicate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define p (condition-predicate &test))");
		l.exec ("(define c (make-condition &test 'aaa 1))");
		l.exec ("(define d (condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		l.exec ("(define e (make-condition &test 'aaa 1))");
		equal(l,"(p c)", T);
		equal(l,"(p d)", T);
		equal(l,"(p e)", T);
	}

	public void testConditionAccessor() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define p (condition-predicate &test))");
		l.exec ("(define d (condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		equal(l,"(condition? d)", T);
		l.exec ("(define d (make-condition &test 'aaa 1))");
		equal(l,"(condition? d)", T);
		equal(l,"(condition? &test)", F);
		equal(l,"(condition? 1)", F);
	}

}
