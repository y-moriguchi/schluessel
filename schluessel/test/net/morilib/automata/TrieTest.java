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

import net.morilib.automata.dfa.DFAs;
import net.morilib.automata.trie.GenericTrie;
import net.morilib.automata.trie.GenericTrieBuilder;
import net.morilib.automata.trie.IntegerTrie;
import net.morilib.automata.trie.IntegerTrieBuilder;
import net.morilib.automata.trie.IntegerTrieMatcher;
import net.morilib.automata.trie.IntegerTrieValueMatcher;
import net.morilib.automata.trie.TrieValueMatcher;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class TrieTest extends TC {

	public void testMatch1() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;

		b.append("cat", 1);
		x = b.get();
		ok(DFAs.isMatched(x, "cat"));
		ng(DFAs.isMatched(x, "can"));
		ng(DFAs.isMatched(x, "at"));
		ng(DFAs.isMatched(x, "ca"));
		eq(DFAs.match(x, "cat").size(), 1);
		ok(DFAs.match(x, "cat").contains(1));
		ok(DFAs.match(x, "can").isEmpty());
		ok(DFAs.match(x, "at").isEmpty());
		ok(DFAs.match(x, "ca").isEmpty());
	}

	public void testMatch2() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;

		b.append("cat", 1).append("can", 2);
		x = b.get();
		ok(DFAs.isMatched(x, "cat"));
		ok(DFAs.isMatched(x, "can"));
		ng(DFAs.isMatched(x, "ca"));
		ng(DFAs.isMatched(x, "at"));
		ng(DFAs.isMatched(x, "an"));
	}

	public void testMatch3() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;

		b.append("cat", 1).append("can", 2).append("cats", 3).append("dog", 4);
		x = b.get();
		ok(DFAs.isMatched(x, "cat"));
		ok(DFAs.isMatched(x, "can"));
		ok(DFAs.isMatched(x, "cats"));
		ok(DFAs.isMatched(x, "dog"));
		ng(DFAs.isMatched(x, "ca"));
		ng(DFAs.isMatched(x, "at"));
		ng(DFAs.isMatched(x, "an"));
	}

	public void testMatcher1() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("cat", 1);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "catcacatccatavcatcat");
		eq(m.nextToken(), "cat");
		eq(m.nextToken(), "cat");
		eq(m.nextToken(), "cat");
		eq(m.nextToken(), "cat");
		eq(m.nextToken(), "cat");
		nil(m.nextToken());
	}

	public void testMatcher2() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivio", 1);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "vivioviviviovviviovivioavivio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivio");
		nil(m.nextToken());
	}

	public void testMatcher3() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivio", 1).append("vio", 2);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "vioviviovivivio");
		eq(m.nextToken(), "vio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivio");
		nil(m.nextToken());
	}

	public void testMatcher4() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivio", 1).append("vingt", 2);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "viviovingtvivingtvvingtvivivio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vingt");
		eq(m.nextToken(), "vingt");
		eq(m.nextToken(), "vingt");
		eq(m.nextToken(), "vivio");
		nil(m.nextToken());
	}

	public void testMatcher5() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivivio", 1).append("vivio", 2).append("vio", 3);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "viovivioviviviovivivivio");
		eq(m.nextToken(), "vio");
		eq(m.nextToken(), "vivio");
		eq(m.nextToken(), "vivivio");
		eq(m.nextToken(), "vivivio");
		nil(m.nextToken());
	}

	public void testMatcher6() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivi", 1).append("vi", 2);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "vivvivi");
		eq(m.nextToken(), "vi");
		eq(m.nextToken(), "vivi");
		nil(m.nextToken());
	}

	public void testMatcher7() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieMatcher<Object> m;

		b.append("vivi", 1).append("vi", 2);
		x = b.get();
		m = new IntegerTrieMatcher<Object>(x, "vivivi");
		eq(m.nextToken(), "vivi");
		eq(m.nextToken(), "vi");
		nil(m.nextToken());
	}

	public void testMatcher106() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieValueMatcher<Object> m;

		b.append("vivi", 1).append("vi", 2);
		x = b.get();
		m = new IntegerTrieValueMatcher<Object>(x, "vivvivi");
		eq(m.nextValue(), 2);
		eq(m.nextValue(), 1);
		nil(m.nextValue());
	}

	public void testMatcher107() {
		IntegerTrieBuilder<Object> b = new IntegerTrieBuilder<Object>();
		IntegerTrie<Object> x;
		IntegerTrieValueMatcher<Object> m;

		b.append("vivi", 1).append("vi", 2);
		x = b.get();
		m = new IntegerTrieValueMatcher<Object>(x, "vivivi");
		eq(m.nextValue(), 1);
		eq(m.nextValue(), 2);
		nil(m.nextValue());
	}

	public void testMatcher201() {
		GenericTrieBuilder<Object, Object> b = new GenericTrieBuilder<Object, Object>();
		GenericTrie<Object, Object> x;
		TrieValueMatcher<Object, Object> m;

		b.append(1, 'c', 'a', 't');
		x = b.get();
		m = new TrieValueMatcher<Object, Object>(x, 'c', 'a', 't', 'c', 'c', 'a', 't');
		eq(m.nextValue(), 1);
		eq(m.nextValue(), 1);
		nil(m.nextValue());
	}

	public void testMatcher207() {
		GenericTrieBuilder<Object, Object> b = new GenericTrieBuilder<Object, Object>();
		GenericTrie<Object, Object> x;
		TrieValueMatcher<Object, Object> m;

		b.append(1, 'v', 'i', 'v', 'i');
		b.append(2, 'v', 'i');
		x = b.get();
		m = new TrieValueMatcher<Object, Object>(x, 'v', 'i', 'v', 'i', 'v', 'i');
		eq(m.nextValue(), 1);
		eq(m.nextValue(), 2);
		nil(m.nextValue());
	}

}
