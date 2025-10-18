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

import java.io.IOException;

import net.morilib.automata.trie.IntegerTrie;
import net.morilib.automata.trie.IntegerTrieTransducer;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/11
 */
public class TrieTransducerTest extends TC {

	static void tt(IntegerTrie<Integer> tr, String a, String b) {
		eq(IntegerTrieTransducer.transduce(tr, a), b);
	}

	public void testT1() throws IOException {
		IntegerTrie<Integer> tr = IntegerTrie.loadCharToString(
				TC.class.getResourceAsStream(
						"/net/morilib/lingua/cyrillic2latin.properties"));

		tt(tr, "Kudryavka", "Кудрявка");
		tt(tr, "Kudryavka!!", "Кудрявка!!");
		tt(tr, "Kud!ryavka", "Куд!рявка");
		tt(tr, "shshcshcchcshch", "шшcшcчcщ");
		tt(tr, "<Kudryavka>", "<Кудрявка>");
	}

}
