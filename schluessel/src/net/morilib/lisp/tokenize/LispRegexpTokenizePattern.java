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
package net.morilib.lisp.tokenize;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.legacy.DFABuilder;
import net.morilib.automata.nfa.NFABuildException;
import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.InputPort;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Tuple2;
import net.morilib.util.tape.CharTape;
import net.morilib.util.tape.MarkableReadOnlyCharTape;
import net.morilib.util.tape.StringTape;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/02
 */
public class LispRegexpTokenizePattern extends Datum2 {

	//
	private DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa;
	private List<Datum> rllist;

	//
	/*package*/ static class LispRegexpTokenizer extends Datum2
	implements ILispTokenizer {

		//
		private LispRegexpTokenizePattern pat;
		private CharTape tape;

		//
		/*package*/ LispRegexpTokenizer(
				LispRegexpTokenizePattern pat,
				CharTape tape) {
			this.pat  = pat;
//			this.stat = pat.dfa.getInitialState();
			this.tape = tape;
		}

		/**
		 * 
		 * @param mesg
		 * @return
		 */
		public Datum tokenize(LispMessage mesg) {
			DFAState<Integer, Integer, Tuple2<Integer, Integer>> stat;
			SortedSet<Integer> acc = new TreeSet<Integer>();
			StringBuilder b = new StringBuilder();
			int pt = tape.mark();

			if(tape.readc() < 0) {
				return MultiValues.newValues(
						LispBoolean.FALSE,
						LispBoolean.FALSE);
			}

			stat = pat.dfa.getInitialState();
			for(int c; (c = tape.readc()) >= 0; tape.moveRight()) {
				b.append((char)c);
				stat = stat.goInt(c);
				if(!stat.getAccepted().isEmpty()) {
					tape.mark();
					acc.clear();
					acc.addAll(stat.getAccepted());
				} else if(stat.isDead()) {
					if(!acc.isEmpty()) {
						int s = acc.first();

						acc.clear();
						b.delete(b.length() - pt + tape.back(),
								b.length());
						tape.moveRight();
						return MultiValues.newValues(
								pat.rllist.get(s),
								new LispString(b.toString()));
					} else {
						throw mesg.getError(
								"err.tokenize.invalidtoken");
					}
				}
				pt++;
			}

			if(!acc.isEmpty()) {
				int s = acc.first();

				b.delete(b.length() - pt + tape.back() + 1,
						b.length());
				tape.moveRight();
				return MultiValues.newValues(
						pat.rllist.get(s),
						new LispString(b.toString()));
			} else {
				throw mesg.getError("err.tokenize.invalidtoken");
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<regexp-tokenizer>");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/02
	 */
	public static class MakeRegexpTokenizePattern extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa;
			ConsIterator itr  = new ConsIterator(c1a);
			List<Datum>  rl   = new ArrayList<Datum>();
			List<String> rexs = new ArrayList<String>();

			while(itr.hasNext()) {
				Datum d = itr.next();

				if(d instanceof Cons) {
					Cons c = (Cons)d;
					String re = SubrUtils.getString(c.getCar(), mesg);

					rexs.add(re);
					rl.add(c.getCdr());
				}
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", c1a);
			}

			try {
				dfa = DFABuilder.getInstance().buildCombined(
						rexs.toArray(new String[0]));
				return new LispRegexpTokenizePattern(dfa, rl);
			} catch(NFABuildException e) {
				throw mesg.getError(
						"err.tokenize.invalidregexp", e.getMessage());
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/02
	 */
	public static class MakeRegexpTokenizerString extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispRegexpTokenizePattern) {
				String str = SubrUtils.getString(c2a, mesg);

				return new LispRegexpTokenizer(
						(LispRegexpTokenizePattern)c1a,
						new StringTape(str));
			} else {
				throw mesg.getError(
						"err.tokenize.require.regexp", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/02
	 */
	public static class MakeRegexpTokenizerPort extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c2a instanceof InputPort)) {
				throw mesg.getError("err.require.iport", c2a);
			} else if(c1a instanceof LispRegexpTokenizePattern) {
				return new LispRegexpTokenizer(
						(LispRegexpTokenizePattern)c1a,
						new MarkableReadOnlyCharTape((InputPort)c2a));
			} else {
				throw mesg.getError(
						"err.tokenize.require.regexp", c1a);
			}
		}

	}

	//
	private LispRegexpTokenizePattern(
			DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa,
			List<Datum> rl) {
		this.dfa    = dfa;
		this.rllist = rl;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<regexp-tokenize-pattern>");
	}

}
