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

import net.morilib.automata.trie.IntegerTrieBuilder;
import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class MakeStringTrie extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		IntegerTrieBuilder<Datum> b = new IntegerTrieBuilder<Datum>();
		Datum d, x;
		String s;

		while(itr.hasNext()) {
			d = itr.next();
			if(d instanceof Cons) {
				s = SubrUtils.getString(((Cons)d).getCar(), mesg);
				x = ((Cons)d).getCdr();
			} else {
				s = SubrUtils.getString(d, mesg);
				x = Nil.NIL;
			}

			if(s.isEmpty()) {
				throw mesg.getError("err.automata.notempty", s);
			}
			b.append(s, x);
		}
		return new LispStringTrie(b.get());
	}

}
