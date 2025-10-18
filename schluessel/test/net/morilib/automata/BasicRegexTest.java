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
package net.morilib.automata;

import net.morilib.automata.regular.AlternativeBasicRegex;
import net.morilib.automata.regular.BasicRegex;
import net.morilib.automata.regular.BasicRegexUtils;
import net.morilib.automata.regular.ConcatenateBasicRegex;
import net.morilib.automata.regular.ObjectBasicRegex;
import net.morilib.automata.regular.StarClosureBasicRegex;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/03
 */
public class BasicRegexTest extends TC {

	public void testStar001() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new StarClosureBasicRegex(b0);
		BasicRegex b  = new StarClosureBasicRegex(b1);

		eq(b.simplify(), b1);
	}

	public void testStar002() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new StarClosureBasicRegex(b0);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b  = new StarClosureBasicRegex(b2);

		eq(b.simplify(), b1);
	}

	public void testStar003() {
		BasicRegex b0 = BasicRegexUtils.NIHIL;
		BasicRegex b  = new StarClosureBasicRegex(b0);

		eq(b.simplify(), BasicRegexUtils.EPSILON);
	}

	public void testStar004() {
		BasicRegex b0 = BasicRegexUtils.EPSILON;
		BasicRegex b  = new StarClosureBasicRegex(b0);

		eq(b.simplify(), b0);
	}

	public void testAlter001() {
		BasicRegex b  = new AlternativeBasicRegex();

		eq(b.simplify(), BasicRegexUtils.EPSILON);
	}

	public void testAlter002() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b  = new AlternativeBasicRegex(b0);

		eq(b.simplify(), b0);
	}

	public void testAlter003() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b  = new AlternativeBasicRegex(b0, b1);

		eq(b.simplify(), b0);
	}

	public void testAlter004() {
		BasicRegex b0 = BasicRegexUtils.NIHIL;
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b  = new AlternativeBasicRegex(b0, b1);

		eq(b.simplify(), b1);
	}

	public void testAlter005() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b  = new AlternativeBasicRegex(b0, b2);

		eq(b.simplify(), b2);
	}

	public void testAlter006() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b  = new AlternativeBasicRegex(b2, b0);

		eq(b.simplify(), b2);
	}

	public void testAlter007() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b0);
		BasicRegex b3 = new StarClosureBasicRegex(b1);
		BasicRegex b  = new AlternativeBasicRegex(b2, b3);

		eq(b.simplify(), b2);
	}

	public void testAlter008() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = new ObjectBasicRegex(2);
		BasicRegex b  = new AlternativeBasicRegex(b3, b0, b2);
		BasicRegex c  = new AlternativeBasicRegex(b3, b2);

		eq(b.simplify(), c);
	}

	public void testAlter009() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = new ObjectBasicRegex(2);
		BasicRegex b  = new AlternativeBasicRegex(b0, b3, b2);
		BasicRegex c  = new AlternativeBasicRegex(b3, b2);

		eq(b.simplify(), c);
	}

	public void testAlter010() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = new ObjectBasicRegex(2);
		BasicRegex b  = new AlternativeBasicRegex(b2, b3, b0);
		BasicRegex c  = new AlternativeBasicRegex(b3, b2);

		eq(b.simplify(), c);
	}

	public void testAlter011() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = new ObjectBasicRegex(2);
		BasicRegex b4 = new StarClosureBasicRegex(b0);
		BasicRegex b  = new AlternativeBasicRegex(b2, b3, b4);
		BasicRegex c  = new AlternativeBasicRegex(b3, b2);

		eq(b.simplify(), c);
	}

	public void testConcat001() {
		BasicRegex b  = new ConcatenateBasicRegex();

		eq(b.simplify(), BasicRegexUtils.EPSILON);
	}

	public void testConcat002() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b  = new ConcatenateBasicRegex(b0);

		eq(b.simplify(), b0);
	}

	public void testConcat003() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(2);
		BasicRegex b2 = BasicRegexUtils.NIHIL;
		BasicRegex b3 = new ObjectBasicRegex(3);
		BasicRegex b  = new ConcatenateBasicRegex(b0, b1, b2, b3);

		eq(b.simplify(), b2);
	}

	public void testConcat004() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(2);
		BasicRegex b2 = BasicRegexUtils.EPSILON;
		BasicRegex b3 = new ObjectBasicRegex(3);
		BasicRegex b  = new ConcatenateBasicRegex(b0, b1, b2, b3);
		BasicRegex c  = new ConcatenateBasicRegex(b0, b1, b3);

		eq(b.simplify(), c);
	}

	public void testConcat012() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(1);
		BasicRegex b2 = new StarClosureBasicRegex(b0);
		BasicRegex b3 = new StarClosureBasicRegex(b1);
		BasicRegex b  = new ConcatenateBasicRegex(b2, b3);

		eq(b.simplify(), b2);
	}

	public void testConcat014() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(2);
		BasicRegex b2 = new ObjectBasicRegex(3);
		BasicRegex b3 = new AlternativeBasicRegex(b1, b2);
		BasicRegex b  = new ConcatenateBasicRegex(b0, b3);
		BasicRegex c0 = new ConcatenateBasicRegex(b0, b1);
		BasicRegex c1 = new ConcatenateBasicRegex(b0, b2);
		BasicRegex c  = new AlternativeBasicRegex(c0, c1);

		eq(b.simplify(), c);
	}

	public void testConcat015() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(2);
		BasicRegex b2 = new ObjectBasicRegex(3);
		BasicRegex b3 = new AlternativeBasicRegex(b1, b2);
		BasicRegex b4 = new ObjectBasicRegex(3);
		BasicRegex b  = new ConcatenateBasicRegex(b0, b3, b4);
		BasicRegex c0 = new ConcatenateBasicRegex(b0, b1, b4);
		BasicRegex c1 = new ConcatenateBasicRegex(b0, b2, b4);
		BasicRegex c  = new AlternativeBasicRegex(c0, c1);

		eq(b.simplify(), c);
	}

	public void testConcat016() {
		BasicRegex b0 = new ObjectBasicRegex(1);
		BasicRegex b1 = new ObjectBasicRegex(2);
		BasicRegex b2 = new ObjectBasicRegex(3);
		BasicRegex b3 = new AlternativeBasicRegex(b1, b2);
		BasicRegex b4 = new ObjectBasicRegex(3);
		BasicRegex b5 = new ObjectBasicRegex(4);
		BasicRegex b6 = new ObjectBasicRegex(5);
		BasicRegex b7 = new AlternativeBasicRegex(b5, b6);
		BasicRegex b  = new ConcatenateBasicRegex(b0, b3, b4, b7);
		BasicRegex c0 = new ConcatenateBasicRegex(b0, b1, b4, b5);
		BasicRegex c1 = new ConcatenateBasicRegex(b0, b2, b4, b5);
		BasicRegex c2 = new ConcatenateBasicRegex(b0, b1, b4, b6);
		BasicRegex c3 = new ConcatenateBasicRegex(b0, b2, b4, b6);
		BasicRegex c  = new AlternativeBasicRegex(c0, c1, c2, c3);

		eq(b.simplify(), c);
	}

	public void testConcat017() {
		BasicRegex b0 = BasicRegexUtils.EPSILON;
		BasicRegex b1 = BasicRegexUtils.EPSILON;
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = BasicRegexUtils.NIHIL;
		BasicRegex b  = new ConcatenateBasicRegex(b0, b2, b3);

		eq(b.simplify(), BasicRegexUtils.NIHIL);
	}

	public void testConcat018() {
		BasicRegex b0 = BasicRegexUtils.EPSILON;
		BasicRegex b1 = BasicRegexUtils.EPSILON;
		BasicRegex b2 = new StarClosureBasicRegex(b1);
		BasicRegex b3 = BasicRegexUtils.NIHIL;
		BasicRegex b4 = new ConcatenateBasicRegex(b0, b2, b3);
		BasicRegex b5 = BasicRegexUtils.NIHIL;
		BasicRegex b  = new AlternativeBasicRegex(b4, b5);

		eq(b.simplify(), BasicRegexUtils.NIHIL);
	}

}
