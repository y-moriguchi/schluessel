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
package net.morilib.lisp.automata.trie;

import net.morilib.automata.trie.GenericTrieBuilder;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class MakeTrie extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body), jtr;
		GenericTrieBuilder<Datum, Datum> b =
				new GenericTrieBuilder<Datum, Datum>();
		ConsListBuilder l;
		Datum d;

		while(itr.hasNext()) {
			jtr = new ConsIterator(d = itr.next());
			if(jtr.hasNext()) {
				l = new ConsListBuilder();
				while(jtr.hasNext())  l.append(jtr.next());
				b.append(new ConsIterator(l.get()), jtr.rest());
			} else {
				throw mesg.getError("err.require.pair", d);
			}
		}
		return new LispTrie(b.get());
	}

}
