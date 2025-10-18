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

import net.morilib.automata.legacy.NFABuilder;
import net.morilib.automata.nfa.NFAAccept;
import net.morilib.automata.nfa.NFAObject;
import net.morilib.automata.nfa.StringNFAMatcher;
import net.morilib.lisp.test.TC;
import net.morilib.util.IntInclementor;
import net.morilib.util.Tuple2;

public class NFATest extends TC {
	
	public void testSimple1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("a");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("a"));
		eq(mch.getMatched(), "a");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("b"));
		ng(mch.match("ab"));
		ng(mch.match("aa"));
		ng(mch.match("ba"));
		ng(mch.match(""));
	}
	
	public void testSimple2() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[b-x]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("b"));
		eq(mch.getMatched(), "b");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("x"));
		eq(mch.getMatched(), "x");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("z"));
		ng(mch.match("bd"));
		ng(mch.match("bz"));
		ng(mch.match("ad"));
		ng(mch.match(""));
	}
	
	public void testSimple3() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[bcd]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("b"));
		eq(mch.getMatched(), "b");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("c"));
		eq(mch.getMatched(), "c");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("d"));
		eq(mch.getMatched(), "d");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("e"));
		ng(mch.match("bd"));
		ng(mch.match("bz"));
		ng(mch.match("ad"));
		ng(mch.match(""));
	}
	
	public void testSimple4() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[bd]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("b"));
		eq(mch.getMatched(), "b");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("d"));
		eq(mch.getMatched(), "d");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("c"));
		ng(mch.match("e"));
		ng(mch.match("bd"));
		ng(mch.match("bz"));
		ng(mch.match("ad"));
		ng(mch.match(""));
	}
	
	public void testSimple5() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[db]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("b"));
		eq(mch.getMatched(), "b");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("d"));
		eq(mch.getMatched(), "d");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("c"));
		ng(mch.match("e"));
		ng(mch.match("bd"));
		ng(mch.match("bz"));
		ng(mch.match("ad"));
		ng(mch.match(""));
	}
	
	public void testSimple6() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[b-df]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("b"));
		eq(mch.getMatched(), "b");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("d"));
		eq(mch.getMatched(), "d");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("f"));
		eq(mch.getMatched(), "f");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("e"));
		ng(mch.match("g"));
		ng(mch.match("bd"));
		ng(mch.match("bz"));
		ng(mch.match("ad"));
		ng(mch.match(""));
	}
	
	public void testSimple7() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("[-()*+]");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("*"));
		eq(mch.getMatched(), "*");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("-"));
		eq(mch.getMatched(), "-");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("**"));
		ng(mch.match(""));
	}
	
	public void testSimple8() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse(".");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("a"));
		eq(mch.getMatched(), "a");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("\n"));
		ok(mch.match("\011"));   // \n - 1
		ok(mch.match("\013"));   // \n + 1
		ng(mch.match("ab"));
		ng(mch.match(""));
	}
	
	public void testConcat1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("ab");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("ab"));
		eq(mch.getMatched(), "ab");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("a"));
		ng(mch.match("b"));
		ng(mch.match("aa"));
		ng(mch.match("ba"));
		ng(mch.match("aba"));
		ng(mch.match("abab"));
		ng(mch.match(""));
	}
	
	public void testAlter1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("a|bc");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("a"));
		eq(mch.getMatched(), "a");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("bc"));
		eq(mch.getMatched(), "bc");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("ac"));
		ng(mch.match("aa"));
		ng(mch.match("ba"));
		ng(mch.match("bca"));
		ng(mch.match("abc"));
		ng(mch.match(""));
	}
	
	public void testStar1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("a*");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("a"));
		eq(mch.getMatched(), "a");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match(""));
		eq(mch.getMatched(), "");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("aaaaa"));
		eq(mch.getMatched(), "aaaaa");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match("ac"));
		ng(mch.match("aab"));
		ng(mch.match("baaa"));
		ng(mch.match("bc"));
	}
	
	public void testPlus1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("a+");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("a"));
		eq(mch.getMatched(), "a");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match("aaaaa"));
		eq(mch.getMatched(), "aaaaa");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ng(mch.match(""));
		ng(mch.match("ac"));
		ng(mch.match("aab"));
		ng(mch.match("baaa"));
		ng(mch.match("bc"));
	}
	
	public void testCapture1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("(a(bc)(d))");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("abcd"));
		eq(mch.getMatched(), "abcd");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "abcd");
		//eq(mch.getCapture(0, 1), "bc");
		//eq(mch.getCapture(0, 2), "d");
		
		ng(mch.match(""));
		ng(mch.match("abcdd"));
		ng(mch.match("dabcd"));
	}
	
	public void testCapture2() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("<(.+)>");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("<br>"));
		eq(mch.getMatched(), "<br>");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "br");
		
		ok(mch.match("<<<^_^>>>"));
		eq(mch.getMatched(), "<<<^_^>>>");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "<<^_^>>");
		
		ng(mch.match(""));
		ng(mch.match("<"));
		ng(mch.match("<>"));
		ng(mch.match("<a"));
		ng(mch.match("a>"));
	}
	
	public void testCapture3() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("<!--(.+)-->");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("<!--comment-->"));
		eq(mch.getMatched(), "<!--comment-->");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "comment");
	}
	
	public void testCapture4() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("(.+),(.+)");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("aaaa,bbbb,cccc"));
		eq(mch.getMatched(), "aaaa,bbbb,cccc");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "aaaa,bbbb");
		//eq(mch.getCapture(0, 1), "cccc");
	}
	
	public void testGroup1() {
		IntInclementor reseq = new IntInclementor();
		NFABuilder<Integer> bld = NFABuilder.newInstance(reseq);
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa =
			bld.parse("(ab)*");
		
		nfa = NFAAccept.newInstance(nfa, 0);
		StringNFAMatcher<Integer> mch =
			new StringNFAMatcher<Integer>(nfa);
		
		ok(mch.match("ababababab"));
		eq(mch.getMatched(), "ababababab");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		//eq(mch.getCapture(0, 0), "aaaa,bbbb");
		//eq(mch.getCapture(0, 1), "cccc");
		
		ok(mch.match("ab"));
		eq(mch.getMatched(), "ab");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
		
		ok(mch.match(""));
		eq(mch.getMatched(), "");
		ok(mch.getResult().contains(0));
		eq(mch.getResult().size(), 1);
	}
	
}
