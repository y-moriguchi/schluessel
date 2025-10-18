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
package net.morilib.lisp.automata.legacy.lalr;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeReduceAction;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.GrammarSymbol;
import net.morilib.automata.lr.LALR1Items;
import net.morilib.automata.lr.LALR1Table;
import net.morilib.automata.lr.LR1Table;
import net.morilib.automata.lr.LRParseException;
import net.morilib.automata.lr.LRParser;
import net.morilib.automata.lr.LexicalAnalyser;
import net.morilib.automata.lr.Nonterminal;
import net.morilib.automata.lr.SemanticAttributes;
import net.morilib.automata.lr.Terminal;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/22
 */
public class LispGrammar extends Datum2 {

	//
	private class Lexer implements LexicalAnalyser<Datum> {

		//
		private Procedure proc;
		private Environment env;
		private LispMessage mesg;
		private Datum before;
		private Datum semval;

		//
		private Lexer(Procedure p, Environment e, LispMessage m) {
			this.proc = p;
			this.env  = e;
			this.mesg = m;
			next();
		}

		//
		private void next() {
			Datum d = Scheme.callva(proc, env, mesg);

			if(d instanceof MultiValues) {
				List<Datum> vs = ((MultiValues)d).getValues();

				if(vs.size() == 0) {
					throw mesg.getError(
							"err.grammar.unknowntermial",
							Nil.NIL);
				} else if(vs.size() > 1) {
					semval = vs.get(1);
				} else {
					semval = Undef.UNDEF;
				}
				before = vs.get(0);
			} else {
				semval = Undef.UNDEF;
				before = d;
			}
		}

		//
		public boolean isEnded() {
			return !before.isTrue();
		}

		//
		public LexicalAnalyser.Token<Datum> nextToken() {
			Terminal t0;
			Datum    s0 = semval;

			if(isEnded()) {
				return LexicalAnalyser.Token.endMarker();
			} else if((t0 = terminals.getValue(before)) == null) {
				throw mesg.getError(
						"err.grammar.unknowntermial", before);
			}
			next();
			return new LexicalAnalyser.Token<Datum>(t0, s0);
		}

	}

	//
	private static class Ter implements Terminal {

	}

	//
	private static class NTer implements Nonterminal {

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/22
	 */
	public static class MakeGrammar extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			ConsIterator    itr = new ConsIterator(c1a);
			List<Datum>     nls = new ArrayList<Datum>();
			List<Datum>     tls = new ArrayList<Datum>();
			List<Procedure> als = new ArrayList<Procedure>();
			List<ContextFreeRule> rls = new ArrayList<ContextFreeRule>();
			LispGrammar     grm = new LispGrammar();
			ContextFreeGrammar cfg;
			LALR1Items itm;
			final Environment ev2 = env;
			final LispMessage mg2 = mesg;

			while(itr.hasNext()) {
				Datum d1 = itr.next();
				ConsIterator it2 = new ConsIterator(d1);
				Datum nt, gl, pr;

				nt = SubrUtils.nextIf(it2, mesg,
						"err.grammar.invalidrule", d1);
				nls.add(nt);
				grm.putIfAbsentNonterminal(nt);

				gl = SubrUtils.nextIf(it2, mesg,
						"err.grammar.invalidrule", d1);
				tls.add(gl);

				pr = SubrUtils.nextIf(it2, mesg,
						"err.grammar.invalidrule", d1);
				if(!(pr instanceof Procedure)) {
					throw mesg.getError("err.require.procedure", pr);
				}
				als.add((Procedure)pr);
			}
			SubrUtils.checkTerminated(itr, c1a, mesg);

			for(int i = 0; i < nls.size(); i++) {
				List<GrammarSymbol> gs;
				ConsIterator it3 = new ConsIterator(tls.get(i));
				ContextFreeRule cfr;

				gs = new ArrayList<GrammarSymbol>();
				while(it3.hasNext()) {
					Datum d3 = it3.next();

					if(grm.nonterminals.containsKey(d3)) {
						gs.add(grm.nonterminals.getValue(d3));
					} else {
						gs.add(grm.putIfAbsentTerminal(d3));
					}
				}
				SubrUtils.checkTerminated(it3, tls.get(i), mesg);

				cfr = new ContextFreeRule(
						grm.nonterminals.getValue(nls.get(i)),
						gs.toArray(new GrammarSymbol[0]));
				rls.add(cfr);
			}

			cfg = ContextFreeGrammar.newInstance(
					new HashSet<ContextFreeRule>(rls),
					START_SYM);
			itm = LALR1Items.newLALR(cfg);
			grm.table = new LALR1Table(itm);

			grm.parser = new LRParser<Datum>(grm.table);
			for(int i = 0; i < rls.size(); i++) {
				ContextFreeReduceAction<Datum> rac;
				final Procedure pc = als.get(i);

				rac = new ContextFreeReduceAction<Datum>() {

					public Datum action(ContextFreeRule event,
							SemanticAttributes<Datum> attrs) {
						int s0 = event.getDerivedSymbolLength();
						Datum[] ds = new Datum[s0];

						for(int i = 0; i < s0; i++) {
							ds[i] = attrs.get(i);
						}
						return Scheme.callva(pc, ev2, mg2, ds);
					}

				};
				grm.parser.setAction(rls.get(i), rac);
			}
			return grm;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/22
	 */
	public static class ParseGrammar extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof LispGrammar)) {
				throw mesg.getError("err.grammar.require.grammar",
						c1a);
			} else if(!(c2a instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", c2a);
			} else {
				try {
					return ((LispGrammar)c1a).parse(
							(Procedure)c2a, env, mesg);
				} catch (LRParseException e) {
					throw mesg.getError("err.grammar.parseerror");
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/24
	 */
	public static class GetConflicts extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispGrammar) {
				LispGrammar g = (LispGrammar)c1a;
				ConsListBuilder b = new ConsListBuilder();

				for(LR1Table.Conflict ci : g.table.getConflicts()) {
					ConsListBuilder b2 = new ConsListBuilder();

					if(ci.isShiftReduce()) {
						b2.append(Symbol.getSymbol("shift-reduce"));
						b2.append(g.terminals.getKey(
								ci.getShiftSymbol()));
						b2.append(g.ruleToList(ci.getReduceRule()));
					} else {
						b2.append(Symbol.getSymbol("reduce-reduce"));
						b2.append(g.ruleToList(ci.getReduceRule()));
						b2.append(g.ruleToList(ci.getReduceRule2()));
					}
					b.append(b2.get());
				}
				return b.get();
			} else {
				throw mesg.getError("err.grammar.require.grammar",
						c1a);
			}
		}

	}

	//
	private static final Nonterminal START_SYM = new NTer();

	//
	private OneToOneSet<Datum, Terminal>    terminals;
	private OneToOneSet<Datum, Nonterminal> nonterminals;
	private LALR1Table table;
	private LRParser<Datum> parser;

	//
	private LispGrammar() {
		terminals    = new HashOneToOneSet<Datum, Terminal>();
		nonterminals = new HashOneToOneSet<Datum, Nonterminal>();
		nonterminals.put(Symbol.getSymbol("S"), START_SYM);
	}

	//
	private Datum parse(Procedure c2a, Environment env,
			LispMessage mesg) throws LRParseException {
		return parser.parse(new Lexer(c2a, env, mesg));
	}

	//
	private Nonterminal putIfAbsentNonterminal(Datum nt) {
		Nonterminal t0 = nonterminals.getValue(nt);

		if(t0 == null) {
			t0 = new NTer();
			nonterminals.put(nt, t0);
		}
		return t0;
	}

	//
	private Terminal putIfAbsentTerminal(Datum d) {
		Terminal t0 = terminals.getValue(d);

		if(t0 == null) {
			t0 = new Ter();
			terminals.put(d, t0);
		}
		return t0;
	}

	//
	private Datum symbolToDatum(GrammarSymbol s) {
		Datum d = terminals.getKey(s);

		if(d == null) {
			d = nonterminals.getKey(s);
		}
		return d;
	}

	//
	private Datum ruleToList(ContextFreeRule r) {
		ConsListBuilder b1 = new ConsListBuilder();
		ConsListBuilder b2 = new ConsListBuilder();

		b1.append(nonterminals.getKey(r.getLeftSymbol()));
		for(int i = 0; i < r.getDerivedSymbolLength(); i++) {
			b2.append(symbolToDatum(r.getDerivedSymbol(i)));
		}
		b1.append(b2.get());
		return b1.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<grammar>");
	}

}
