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

import java.util.Arrays;
import java.util.HashSet;

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispConditionType;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/10
 */
public class SRFI35Test extends TCSubr {

	public void testMakeConditionType() {
		Scheme l = Scheme.newInstance();
		LispConditionType t;

		t = (LispConditionType)l.exec(
				"(make-condition-type '&test &condition '(aaa bbb))");
		eq(t.getId(), "&test");
		eq(t.getParent().getId(), "&condition");
		eq(t.getFieldNames(), new HashSet<String>(Arrays.asList("aaa", "bbb")));

		t = (LispConditionType)l.exec(
				"(make-condition-type '&test &condition '())");
		eq(t.getId(), "&test");
		eq(t.getParent().getId(), "&condition");
		eq(t.getFieldNames(), new HashSet<String>());
	}

	public void testIsConditionType() {
		Scheme l = Scheme.newInstance();

		equal(l,"(condition-type? &condition)", T);
		equal(l,"(condition-type? (make-condition &condition))", F);
		equal(l,"(condition-type? 1)", F);
	}

	public void testMakeCondition() {
		Scheme l = Scheme.newInstance();
		LispCondition c;

		l.exec ("(define &test (make-condition-type '&test &condition '(aaa bbb)))");
		c = (LispCondition)l.exec("(make-condition &condition)");
		ok(c.hasType(LispConditionType.CONDITION));

		c = (LispCondition)l.exec("(make-condition &test 'aaa 1)");
		ok(c.hasType(LispConditionType.getInstance("&test")));
		eq(c.getField("aaa"), LispInteger.valueOf(1));
		nil(c.getField("bbb"));
	}

	public void testIsCondition() {
		Scheme l = Scheme.newInstance();

		equal(l,"(condition? &condition)", F);
		equal(l,"(condition? (make-condition &condition))", T);
		equal(l,"(condition? 1)", F);
	}

	public void testIsConditionHasType() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define c (make-condition &test 'aaa 1))");
		equal(l,"(condition-has-type? c &test)", T);
		equal(l,"(condition-has-type? c &test2)", F);
		equal(l,"(condition-has-type? c &condition)", T);
		equal(l,"(condition-has-type? c &error)", F);
		l.exec ("(define d (make-compound-condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 1)))");
		equal(l,"(condition-has-type? d &test)", T);
		equal(l,"(condition-has-type? d &test2)", T);
		equal(l,"(condition-has-type? d &condition)", T);
		equal(l,"(condition-has-type? d &error)", F);
	}

	public void testConditionRef() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define c (make-condition &test 'aaa 1))");
		l.exec ("(define d (make-compound-condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		eqi  (l,"(condition-ref c 'aaa)", 1);
		eqi  (l,"(condition-ref d 'aaa)", 1);
		eqi  (l,"(condition-ref d 'ccc)", 2);
		lperr(l,"(condition-ref c 'ccc)");
		lperr(l,"(condition-ref d 'eee)");
	}

	public void testMakeCompoundCondition() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define d (make-compound-condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		equal(l,"(condition-has-type? d &test)", T);
		equal(l,"(condition-has-type? d &test2)", T);
		equal(l,"(condition-has-type? d &condition)", T);
		equal(l,"(condition-has-type? d &error)", F);
		eqi  (l,"(condition-ref d 'aaa)", 1);
		eqi  (l,"(condition-ref d 'ccc)", 2);
	}

	public void testExtractCondition() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define c (make-condition &test 'aaa 1))");
		l.exec ("(define d (make-compound-condition" +
				"  (make-condition &test 'aaa 1)" +
				"  (make-condition &test2 'ccc 2)))");
		equal(l,"(condition-has-type? (extract-condition c &test) &test)", T);
		equal(l,"(extract-condition c &test2)", F);
		equal(l,"(condition-has-type? (extract-condition d &test) &test)", T);
		equal(l,"(condition-has-type? (extract-condition d &test2) &test2)", T);
		equal(l,"(extract-condition d &error)", F);
	}

	public void testDefineConditionType() {
		Scheme l = Scheme.newInstance();
		LispConditionType t;

		l.exec ("(define-condition-type &test &condition test-condition?" +
				"  (aaa get-aaa) (bbb get-bbb))");
		t = (LispConditionType)l.get("&test");
		eq(t.getId(), "&test");
		eq(t.getParent().getId(), "&condition");
		eq(t.getFieldNames(), new HashSet<String>(Arrays.asList("aaa", "bbb")));
		l.exec ("(define c (make-condition &test 'aaa 1 'bbb 2))");
		eqi  (l,"(get-aaa c)", 1);
		eqi  (l,"(get-bbb c)", 2);
		equal(l,"(test-condition? c)", T);

		l.exec ("(define-condition-type &test &condition test-condition?)");
		t = (LispConditionType)l.get("&test");
		eq(t.getId(), "&test");
		eq(t.getParent().getId(), "&condition");
		eq(t.getFieldNames(), new HashSet<String>());
		l.exec ("(define c (make-condition &test))");
		equal(l,"(test-condition? c)", T);
	}

	public void testConditionSrfi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define &test  (make-condition-type '&test  &condition '(aaa bbb)))");
		l.exec ("(define &test2 (make-condition-type '&test2 &condition '(ccc ddd)))");
		l.exec ("(define d (condition-srfi" +
				"  (&test  (aaa 1) (bbb 2))" +
				"  (&test2 (ccc 3) (ddd 4))))");
		eqi  (l,"(condition-ref d 'aaa)", 1);
		eqi  (l,"(condition-ref d 'bbb)", 2);
		eqi  (l,"(condition-ref d 'ccc)", 3);
		eqi  (l,"(condition-ref d 'ddd)", 4);
	}

}
