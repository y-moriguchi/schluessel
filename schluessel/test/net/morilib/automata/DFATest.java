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
package net.morilib.automata;

import java.util.Set;

import net.morilib.automata.CharSequenceHead;
import net.morilib.automata.dfa.DFAs;
import net.morilib.automata.legacy.DFABuilder;
import net.morilib.lisp.test.TC;
import net.morilib.util.Tuple2;

public class DFATest extends TC {

	public void testSimple1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("a");

		Set<Void> st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "b").isEmpty());
		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "ba").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple2() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[b-x]");
		Set<Void> st;

		st = DFAs.input(dfa, "b");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "x");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "z").isEmpty());
		ok(DFAs.input(dfa, "bd").isEmpty());
		ok(DFAs.input(dfa, "bz").isEmpty());
		ok(DFAs.input(dfa, "ad").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple3() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[bcd]");
		Set<Void> st;

		st = DFAs.input(dfa, "b");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "c");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "d");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "e").isEmpty());
		ok(DFAs.input(dfa, "bd").isEmpty());
		ok(DFAs.input(dfa, "bz").isEmpty());
		ok(DFAs.input(dfa, "ad").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple4() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[bd]");
		Set<Void> st;

		st = DFAs.input(dfa, "b");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "d");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "c").isEmpty());
		ok(DFAs.input(dfa, "e").isEmpty());
		ok(DFAs.input(dfa, "bd").isEmpty());
		ok(DFAs.input(dfa, "bz").isEmpty());
		ok(DFAs.input(dfa, "ad").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple5() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[db]");
		Set<Void> st;

		st = DFAs.input(dfa, "b");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "d");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "c").isEmpty());
		ok(DFAs.input(dfa, "e").isEmpty());
		ok(DFAs.input(dfa, "bd").isEmpty());
		ok(DFAs.input(dfa, "bz").isEmpty());
		ok(DFAs.input(dfa, "ad").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple6() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[b-df]");
		Set<Void> st;

		st = DFAs.input(dfa, "b");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "d");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "f");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "e").isEmpty());
		ok(DFAs.input(dfa, "g").isEmpty());
		ok(DFAs.input(dfa, "bd").isEmpty());
		ok(DFAs.input(dfa, "bz").isEmpty());
		ok(DFAs.input(dfa, "ad").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple7() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[-()*+]");
		Set<Void> st;

		st = DFAs.input(dfa, "*");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "-");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "**").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple8() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build(".");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "\n").isEmpty());
		ng(DFAs.input(dfa, "\011").isEmpty());   // \n - 1
		ng(DFAs.input(dfa, "\013").isEmpty());   // \n + 1
		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple9() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[acegikm]");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "e");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "m");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "b").isEmpty());
		ok(DFAs.input(dfa, "d").isEmpty());
		ok(DFAs.input(dfa, "f").isEmpty());
		ok(DFAs.input(dfa, "h").isEmpty());
		ok(DFAs.input(dfa, "j").isEmpty());
		ok(DFAs.input(dfa, "l").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple10() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[mkigceace]");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "e");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "m");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "b").isEmpty());
		ok(DFAs.input(dfa, "d").isEmpty());
		ok(DFAs.input(dfa, "f").isEmpty());
		ok(DFAs.input(dfa, "h").isEmpty());
		ok(DFAs.input(dfa, "j").isEmpty());
		ok(DFAs.input(dfa, "l").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testSimple11() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[^b-x]");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "z");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "b").isEmpty());
		ok(DFAs.input(dfa, "x").isEmpty());
		ok(DFAs.input(dfa, "az").isEmpty());
		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "ba").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testConcat1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("ab");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "ab");
		ok(st.contains(null));
		eq(st.size(), 1);

		ch = new CharSequenceHead("abab");
		st = DFAs.match(dfa, ch);
		ok(st.contains(null));
		eq(st.size(), 1);
		st = DFAs.match(dfa, ch);
		ok(st.contains(null));
		eq(st.size(), 1);
		ok(DFAs.match(dfa, ch).isEmpty());

		ch = new CharSequenceHead("aba");
		st = DFAs.match(dfa, ch);
		ok(st.contains(null));
		eq(st.size(), 1);
		ok(DFAs.match(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "a").isEmpty());
		ok(DFAs.input(dfa, "b").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "ba").isEmpty());
		ok(DFAs.input(dfa, "aba").isEmpty());
		ok(DFAs.input(dfa, "abab").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testAlter1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("a|bc");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "bc");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "ac").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "ba").isEmpty());
		ok(DFAs.input(dfa, "bca").isEmpty());
		ok(DFAs.input(dfa, "abc").isEmpty());
		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testStar1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("a*");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "aaaaa");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "ac").isEmpty());
		ok(DFAs.input(dfa, "aab").isEmpty());
		ok(DFAs.input(dfa, "baaa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testPlus1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("a+");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "aaaaa");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "").isEmpty());
		ok(DFAs.input(dfa, "ac").isEmpty());
		ok(DFAs.input(dfa, "aab").isEmpty());
		ok(DFAs.input(dfa, "baaa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testQuestion1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("a?");
		Set<Void> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "aaaaa").isEmpty());
		ok(DFAs.input(dfa, "ac").isEmpty());
		ok(DFAs.input(dfa, "aab").isEmpty());
		ok(DFAs.input(dfa, "baaa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testCapture1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("(a(bc)(d))");
		Set<Void> st;

		st = DFAs.input(dfa, "abcd");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "abcd");
		//eq(mch.getCapture(0, 1), "bc");
		//eq(mch.getCapture(0, 2), "d");

		ok(DFAs.input(dfa, "").isEmpty());
		ok(DFAs.input(dfa, "abcdd").isEmpty());
		ok(DFAs.input(dfa, "dabcd").isEmpty());
	}

	public void testCapture2() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("<(.+)>");
		Set<Void> st;

		st = DFAs.input(dfa, "<br>");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "br");

		st = DFAs.input(dfa, "<<<^_^>>>");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "<<^_^>>");

		ok(DFAs.input(dfa, "").isEmpty());
		ok(DFAs.input(dfa, "<").isEmpty());
		ok(DFAs.input(dfa, "<>").isEmpty());
		ok(DFAs.input(dfa, "<a").isEmpty());
		ok(DFAs.input(dfa, "a>").isEmpty());
	}

	public void testCapture3() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("<!--(.+)-->");
		Set<Void> st;

		st = DFAs.input(dfa, "<!--comment-->");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");
	}

	public void testCapture4() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("(.+),(.+)");
		Set<Void> st;

		st = DFAs.input(dfa, "aaaa,bbbb,cccc");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "aaaa,bbbb");
		//eq(mch.getCapture(0, 1), "cccc");
	}

	public void testGroup1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("(ab)*");
		Set<Void> st;

		st = DFAs.input(dfa, "ababababab");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "aaaa,bbbb");
		//eq(mch.getCapture(0, 1), "cccc");

		st = DFAs.input(dfa, "ab");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "");
		ok(st.contains(null));
		eq(st.size(), 1);
	}

	public void testGroup2() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[a-f]+[d-z]+");
		Set<Void> st;

		st = DFAs.input(dfa, "abcdefdghij");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ok(DFAs.input(dfa, "defghidefabc").isEmpty());
		ok(DFAs.input(dfa, "aaaaaaaa").isEmpty());
	}

	public void testGroup3() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[d-z]+[a-f]+");
		Set<Void> st;

		st = DFAs.input(dfa, "defghidefabc");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ok(DFAs.input(dfa, "abcdefdghij").isEmpty());
		ok(DFAs.input(dfa, "zzzzzzzz").isEmpty());
	}

	public void testGroup4() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("[efgh]+A|[a-f]+[d-z]+");
		Set<Void> st;

		st = DFAs.input(dfa, "abcdefdghij");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		st = DFAs.input(dfa, "efhgggehA");
		ok(st.contains(null));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "defghidefabc").isEmpty());
		ok(DFAs.input(dfa, "efhggghejA").isEmpty());
		ok(DFAs.input(dfa, "aaaaaaaa").isEmpty());
	}

	public void testBound1() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("^abc");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "abc");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ch = new CharSequenceHead("aabc");
		ch.readInt();
		ok(DFAs.input(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testBound2() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("^abc|^de");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "abc");
		ok(st.contains(null));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "de");
		ok(st.contains(null));
		eq(st.size(), 1);

		ch = new CharSequenceHead("aabc");
		ch.readInt();
		ok(DFAs.input(dfa, ch).isEmpty());

		ch = new CharSequenceHead("dde");
		ch.readInt();
		ok(DFAs.input(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "dd").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
		ok(DFAs.input(dfa, "ee").isEmpty());
	}

	public void testBound3() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("abc$");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "abc");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ch = new CharSequenceHead("abca");
		ch.readInt();
		ok(DFAs.match(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testBound4() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("^abc$");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "abc");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ch = new CharSequenceHead("aabc");
		ch.readInt();
		ok(DFAs.input(dfa, ch).isEmpty());

		ch = new CharSequenceHead("abca");
		ch.readInt();
		ok(DFAs.match(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testBound5() {
		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			DFABuilder.getInstance().build("^$");
		Set<Void> st;
		CharSequenceHead ch;

		st = DFAs.input(dfa, "");
		ok(st.contains(null));
		eq(st.size(), 1);
		//eq(mch.getCapture(0, 0), "comment");

		ch = new CharSequenceHead("a");
		ch.readInt();
		ok(DFAs.match(dfa, ch).isEmpty());

		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "bc").isEmpty());
	}

	public void testCombined1() {
		DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa =
			DFABuilder.getInstance().buildCombined("a", "bc");
		Set<Integer> st;

		st = DFAs.input(dfa, "a");
		ok(st.contains(0));
		eq(st.size(), 1);

		st = DFAs.input(dfa, "bc");
		ok(st.contains(1));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "").isEmpty());
		ok(DFAs.input(dfa, "ab").isEmpty());
		ok(DFAs.input(dfa, "aa").isEmpty());
		ok(DFAs.input(dfa, "be").isEmpty());
		ok(DFAs.input(dfa, "ebc").isEmpty());
	}

	public void testCombined2() {
		DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa =
			DFABuilder.getInstance().buildCombined("a[b-x]", "abc");
		Set<Integer> st;
		CharSequenceHead ch;

		ch = new CharSequenceHead("abcab");
		st = DFAs.match(dfa, ch);
		ok(st.contains(1));
		eq(st.size(), 1);

		st = DFAs.match(dfa, ch);
		ok(st.contains(0));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "").isEmpty());
	}

	public void testCombined3() {
		DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa =
			DFABuilder.getInstance().buildCombined("[a-z]+", "abc");
		Set<Integer> st;
		CharSequenceHead ch;

		ch = new CharSequenceHead("abc");
		st = DFAs.match(dfa, ch);
		ok(st.contains(0));
		ok(st.contains(1));
		eq(st.size(), 2);

		ch = new CharSequenceHead("abcd");
		st = DFAs.match(dfa, ch);
		ok(st.contains(0));
		eq(st.size(), 1);

		ok(DFAs.input(dfa, "").isEmpty());
	}

}
