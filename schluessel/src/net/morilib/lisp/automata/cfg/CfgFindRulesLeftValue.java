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
package net.morilib.lisp.automata.cfg;

import java.util.Set;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.automata.LispGrammarVariable;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/12
 */
public class CfgFindRulesLeftValue extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		ConsListBuilder b = new ConsListBuilder();
		Set<LispCFGRule> s;

		if(!(c2a instanceof LispGrammarVariable)) {
			throw mesg.getError("err.automata.require.variable", c2a);
		} else if(c1a instanceof LispCFG) {
			s = ((LispCFG)c1a).getRules();
			for(LispCFGRule l : s) {
				if(l.getLeftValue().equals(c2a))  b.append(l);
			}
			return b.get();
		} else {
			throw mesg.getError("err.automata.require.cfg", c1a);
		}
	}

}
