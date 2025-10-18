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
package net.morilib.automata.legacy;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import net.morilib.automata.CharsetBuildException;
import net.morilib.automata.TextBound;
import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeReduceAction;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.LALR1Items;
import net.morilib.automata.lr.LALR1Table;
import net.morilib.automata.lr.LRParseException;
import net.morilib.automata.lr.LRParser;
import net.morilib.automata.lr.LexicalAnalyser;
import net.morilib.automata.lr.Nonterminal;
import net.morilib.automata.lr.SemanticAttributes;
import net.morilib.automata.lr.Terminal;
import net.morilib.automata.nfa.NFAAlternative;
import net.morilib.automata.nfa.NFABuildException;
import net.morilib.automata.nfa.NFAConcatenation;
import net.morilib.automata.nfa.NFAEpsilonBound;
import net.morilib.automata.nfa.NFAObject;
import net.morilib.automata.nfa.NFAOptional;
import net.morilib.automata.nfa.NFAParenthesis;
import net.morilib.automata.nfa.NFARepetition;
import net.morilib.automata.nfa.SingleSetNFA;
import net.morilib.range.Interval;
import net.morilib.range.Intervals;
import net.morilib.range.IntervalsInt;
import net.morilib.range.Range;
import net.morilib.util.Inclementor;
import net.morilib.util.IntInclementor;
import net.morilib.util.Tuple2;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class NFABuilder<A> {

	//
	private static class Term implements Terminal {

		String sym;

		private Term(String s) {
			sym = s;
		}

		public String toString() {
			return sym;
		}

	}

	//
	private static class NTer implements Nonterminal {

		String sym;

		private NTer(String s) {
			sym = s;
		}

		public String toString() {
			return sym;
		}

	}

	//
	private static class Attr<A> {
		private NFAObject<Integer, A, Tuple2<A, Integer>> nfa;
		private Range charset;
		private char  chr;
		private int   cno;
	}

	//
	private static class Lexer<A> implements LexicalAnalyser<Attr<A>> {

		private CharSequence seq;
		private Map<Character, Term> tset = RE_TOKENS;
		private int ptr = 0;
		private boolean befLparl  = false;
		private boolean chsethead = false;

		private Lexer(CharSequence seq) {
			this.seq = seq;
		}

		public boolean isEnded() {
			return ptr >= seq.length();
		}

		public LexicalAnalyser.Token<Attr<A>> nextToken() {
			if(isEnded()) {
				//throw new NoTokensException();
				return Token.endMarker();
			}

			char c = seq.charAt(ptr++);
			Term t = tset.get(c);
			Attr<A> atr = new Attr<A>();

			if(tset == RE_TOKENS) {
				if(t == LPARL) {
					befLparl = chsethead = true;
					tset = CH_TOKENS;
				} else if(t == BSLA) {
					t = LETTR;
					if(isEnded()) {
						atr.nfa = getLtr(c);
					} else {
						int c2 = seq.charAt(ptr++);

						switch(c2) {
						case 't':
							atr.nfa = getLtr('\t');  break;
						case 'n':
							atr.nfa = getLtr('\n');  break;
						default:
							atr.nfa = getLtr(c2);  break;
						}
					}
				} else if(t == null) {
					t = LETTR;
					atr.nfa = getLtr(c);
				}
			} else if(tset == CH_TOKENS) {
				if(t == CARET && !befLparl) {
					t = LETTR;  atr.chr = c;
					chsethead = false;
				} else if(t == MINUS && befLparl) {
					t = LETTR;  atr.chr = c;
					chsethead = false;
				} else if(t == null) {
					t = LETTR;  atr.chr = c;
					chsethead = false;
				} else if(t == RPARL) {
					if(chsethead) {
						t = LETTR;  atr.chr = c;
					} else {
						tset = RE_TOKENS;
					}
					chsethead = false;
				}
				befLparl = false;
			}
			return new LexicalAnalyser.Token<Attr<A>>(t, atr);
		}

	}

	//
	private static class NBld<A> extends NFABuilder<A> {

		private Inclementor<A> reseq;
		private Inclementor<Integer> capseq;

		private NBld(Inclementor<A> reseq) {
			this.parser = createParser();
			this.reseq  = reseq;
		}

		//
		private LRParser<Attr<A>> createParser() {
			LRParser<Attr<A>> parser = new LRParser<Attr<A>>(PTABLE);

			parser.setAction(
					S1, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(0);
					Attr<A> r2 = attrs.get(2);
					Attr<A> re = new Attr<A>();

					re.nfa = NFAAlternative.newInstance(
							r1.nfa, r2.nfa);
					return re;
				}

			});

			parser.setAction(
					S2, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					return attrs.get(0);
				}

			});

			parser.setAction(
					L1, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(0);
					Attr<A> r2 = attrs.get(1);
					Attr<A> re = new Attr<A>();

					re.nfa = NFAConcatenation.newInstance(
							r1.nfa, r2.nfa);
					return re;
				}

			});

			parser.setAction(
					L2, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					return attrs.get(0);
				}

			});

			parser.setAction(
					T1, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(0);
					Attr<A> re = new Attr<A>();

					re.nfa = NFARepetition.newInstance(r1.nfa, true);
					return re;
				}

			});

			parser.setAction(
					T2, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(0);
					Attr<A> re = new Attr<A>();

					re.nfa = NFARepetition.newInstance(r1.nfa, false);
					return re;
				}

			});

			parser.setAction(
					T3, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					return attrs.get(0);
				}

			});

			parser.setAction(
					T4, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(0);
					Attr<A> re = new Attr<A>();

					re.nfa = NFAOptional.newInstance(r1.nfa);
					return re;
				}

			});

			parser.setAction(
					F1, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(1);
					Attr<A> re = new Attr<A>();
					A       rq = reseq.getObject();
					int     sq = attrs.get(0).cno;

					re.nfa = NFAParenthesis.newInstance(
							r1.nfa,
							new Tuple2<A, Integer>(rq, sq));
					return re;
				}

			});

			parser.setAction(
					FP, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> re = new Attr<A>();

					re.cno = capseq.getObject();
					capseq.suc();
					return re;
				}

			});

			parser.setAction(
					F2, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					return attrs.get(0);
				}

			});

			parser.setAction(
					F3, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> r1 = attrs.get(1);
					Attr<A> re = new Attr<A>();

					re.nfa = SingleSetNFA.newInstance(r1.charset);
					return re;
				}

			});

			parser.setAction(
					F4, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> re = new Attr<A>();

					re.nfa = SingleSetNFA.newInstance(DOTSET);
					return re;
				}

			});

			parser.setAction(
					F5, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> re = new Attr<A>();

					re.nfa = NFAEpsilonBound.newInstance(
							TextBound.BEGIN_LINE);
					return re;
				}

			});

			parser.setAction(
					F6, new ContextFreeReduceAction<Attr<A>>() {

				public Attr<A> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<A>> attrs) {
					Attr<A> re = new Attr<A>();

					re.nfa = NFAEpsilonBound.newInstance(
							TextBound.END_LINE);
					return re;
				}

			});

			// charset
			addCharsetAction(parser);

			return parser;
		}

		public NFAObject<Integer, A, Tuple2<A, Integer>> parse(
				CharSequence seq) {
			capseq = new IntInclementor();

			try {
				Attr<A> res = parser.parse(new Lexer<A>(seq));

				return res.nfa;
			} catch (LRParseException e) {
				throw new NFABuildException(seq.toString());
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2009
	 */
	public static final class CharsetBuilderImpl
	extends CharsetBuilder {

		private LRParser<Attr<Void>> parser;

		public CharsetBuilderImpl() {
			parser = new LRParser<Attr<Void>>(PTABLE_CHR);

			parser.setAction(
					SC, new ContextFreeReduceAction<Attr<Void>>() {

				public Attr<Void> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<Void>> attrs) {
					Attr<Void> r1 = attrs.get(1);
					Attr<Void> re = new Attr<Void>();

					re.charset = r1.charset;
					return re;
				}

			});

			parser.setAction(
					SD, new ContextFreeReduceAction<Attr<Void>>() {

				public Attr<Void> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<Void>> attrs) {
					Attr<Void> re = new Attr<Void>();

					re.charset = Interval.O;
					return re;
				}

			});

			parser.setAction(
					C1, new ContextFreeReduceAction<Attr<Void>>() {

				public Attr<Void> action(
						ContextFreeRule event,
						SemanticAttributes<Attr<Void>> attrs) {
					Attr<Void> re = new Attr<Void>();
					Attr<Void> r1 = attrs.get(1);

					re.charset = r1.charset.complement(
							IntervalsInt.newRightOpenInterval(
									0, Character.MAX_VALUE + 1));
					return re;
				}

			});
		}

		@Override
		public Range parse(CharSequence seq) {
			try {
				Attr<Void> res = parser.parse(new Lexer<Void>(seq));

				return res.charset;
			} catch (LRParseException e) {
				throw new CharsetBuildException(e.getMessage());
			}
		}

	}

	//
	private static final Term LETTR = new Term("c");
	private static final Term LPAR  = new Term("(");
	private static final Term RPAR  = new Term(")");
	private static final Term STAR  = new Term("*");
	private static final Term PLUS  = new Term("+");
	private static final Term BAR   = new Term("|");
	private static final Term LPARL = new Term("[");
	private static final Term DOT   = new Term(".");
	private static final Term RPARL = new Term("]");
	private static final Term MINUS = new Term("-");
	private static final Term CARET = new Term("^");
	private static final Term QUES  = new Term("?");
	private static final Term DOLL  = new Term("$");
	private static final Term BSLA  = new Term("\\");

	private static final Map<Character, Term> RE_TOKENS;
	private static final Map<Character, Term> CH_TOKENS;
	private static final LALR1Table PTABLE;
	private static final LALR1Table PTABLE_CHR;

	private static final ContextFreeRule S1, S2;
	private static final ContextFreeRule L1, L2;
	private static final ContextFreeRule T1, T2, T3, T4;
	private static final ContextFreeRule F1, FP, F2, F3, F4, F5, F6;
	private static final ContextFreeRule C1, C2;
	private static final ContextFreeRule D1, D2;
	private static final ContextFreeRule E1, E2;
	private static final ContextFreeRule SC, SD;

	private static final Range ALLSET = Intervals.newClosedInterval(
			Integer.valueOf(0), Integer.valueOf(Integer.MAX_VALUE));
	private static final Range DOTSET;

	/*package*/ LRParser<Attr<A>> parser;

	//
	static {
		Map<Character, Term> rt = new HashMap<Character, Term>();
		Map<Character, Term> ct = new HashMap<Character, Term>();

		rt.put('(', LPAR);
		rt.put(')', RPAR);
		rt.put('*', STAR);
		rt.put('+', PLUS);
		rt.put('|', BAR);
		rt.put('[', LPARL);
		rt.put('.', DOT);
		rt.put('?', QUES);
		rt.put('^', CARET);
		rt.put('$', DOLL);
		rt.put('\\', BSLA);

		ct.put(']', RPARL);
		ct.put('-', MINUS);
		ct.put('^', CARET);
		//rt.put('[', LPARL);

		RE_TOKENS = Collections.unmodifiableMap(rt);
		CH_TOKENS = Collections.unmodifiableMap(ct);

		// charset ----------------------------------------------------
		HashSet<ContextFreeRule> rchs = new HashSet<ContextFreeRule>();

		NTer sc = new NTer("SC");
		NTer c  = new NTer("C");
		NTer d  = new NTer("D");
		NTer e  = new NTer("E");

		// SC := [ C ]
		SC = new ContextFreeRule(sc, LPARL, c, RPARL);

		// SC := [ ]
		SD = new ContextFreeRule(sc, LPARL, RPARL);

		// C := ^ D
		C1 = new ContextFreeRule(c, CARET, d);

		// C := D
		C2 = new ContextFreeRule(c, d);

		// D := D E
		D1 = new ContextFreeRule(d, d, e);

		// D := E
		D2 = new ContextFreeRule(d, e);

		// E := char - char
		E1 = new ContextFreeRule(e, LETTR, MINUS, LETTR);

		// E := char
		E2 = new ContextFreeRule(e, LETTR);

		rchs.add(SC);  rchs.add(SD);
		rchs.add(C1);  rchs.add(C2);
		rchs.add(D1);  rchs.add(D2);
		rchs.add(E1);  rchs.add(E2);

		ContextFreeGrammar g2 =
			ContextFreeGrammar.newInstance(rchs, sc);
		LALR1Items items2 = LALR1Items.newLALR(g2);
		PTABLE_CHR = new LALR1Table(items2);

		// build regular expression -----------------------------------
		HashSet<ContextFreeRule> rreg = new HashSet<ContextFreeRule>();

		NTer s  = new NTer("S");
		NTer l  = new NTer("L");
		NTer lp = new NTer("LP");
		NTer t  = new NTer("T");
		NTer f  = new NTer("F");

		// S := S | L
		S1 = new ContextFreeRule(s, s, BAR, l);

		// S := L
		S2 = new ContextFreeRule(s, l);

		// L := L T
		L1 = new ContextFreeRule(l, l, t);

		// L := T
		L2 = new ContextFreeRule(l, t);

		// T := F *
		T1 = new ContextFreeRule(t, f, STAR);

		// T := F +
		T2 = new ContextFreeRule(t, f, PLUS);

		// T := F
		T3 = new ContextFreeRule(t, f);

		// T := F ?
		T4 = new ContextFreeRule(t, f, QUES);

		// F := LP S )
		F1 = new ContextFreeRule(f, lp, s, RPAR);

		// LP := (
		FP = new ContextFreeRule(lp, LPAR);

		// F := char
		F2 = new ContextFreeRule(f, LETTR);

		// F := [ C ]
		F3 = new ContextFreeRule(f, LPARL, c, RPARL);

		// F := .
		F4 = new ContextFreeRule(f, DOT);

		// F := ^
		F5 = new ContextFreeRule(f, CARET);

		// F := $
		F6 = new ContextFreeRule(f, DOLL);

		rreg.add(S1);  rreg.add(S2);
		rreg.add(L1);  rreg.add(L2);
		rreg.add(T1);  rreg.add(T2);  rreg.add(T3);  rreg.add(T4);
		rreg.add(F1);  rreg.add(FP);
		rreg.add(F2);  rreg.add(F3);  rreg.add(F4);
		rreg.add(F5);  rreg.add(F6);
		rreg.add(C1);  rreg.add(C2);
		rreg.add(D1);  rreg.add(D2);
		rreg.add(E1);  rreg.add(E2);

		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rreg, s);
		LALR1Items items = LALR1Items.newLALR(g);
		PTABLE = new LALR1Table(items);

		// 
		DOTSET = Intervals.newRightOpenInterval(
				Integer.valueOf('\n'),
				Integer.valueOf('\n' + 1)).complement(ALLSET);
	}

	//
	/*package*/ NFABuilder() {
		// do nothing
	}

	//
	private static<A> NFAObject<Integer, A, Tuple2<A, Integer>> getLtr(
			int c) {
		//atr.nfa = SingleObjectNFA.newInstance((int)c);
		Interval rz = Intervals.newRightOpenInterval(
				Integer.valueOf((int)c),
				Integer.valueOf((int)c + 1));
		return SingleSetNFA.newInstance(rz);
	}

	//
	private static<A> void addCharsetAction(LRParser<Attr<A>> parser) {
		parser.setAction(C1, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				Attr<A> r1 = attrs.get(1);
				Attr<A> re = new Attr<A>();

				re.charset = r1.charset.complement(ALLSET);
				return re;
			}

		});

		parser.setAction(C2, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				return attrs.get(0);
			}

		});

		parser.setAction(D1, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				Attr<A> r1 = attrs.get(0);
				Attr<A> r2 = attrs.get(1);
				Attr<A> re = new Attr<A>();

				re.charset = r1.charset.join(r2.charset);
				return re;
			}

		});

		parser.setAction(D2, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				return attrs.get(0);
			}

		});

		parser.setAction(E1, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				Attr<A> r1 = attrs.get(0);
				Attr<A> r2 = attrs.get(2);
				Attr<A> re = new Attr<A>();
				Range rz;

				if(r1.chr > r2.chr) {
					throw new NFABuildException();
				}

				rz = Intervals.newRightOpenInterval(
						Integer.valueOf(r1.chr),
						Integer.valueOf(r2.chr + 1));
				re.charset = rz;
				return re;
			}

		});

		parser.setAction(E2, new ContextFreeReduceAction<Attr<A>>() {

			public Attr<A> action(
					ContextFreeRule event,
					SemanticAttributes<Attr<A>> attrs) {
				Attr<A> r1 = attrs.get(0);
				Attr<A> re = new Attr<A>();
				Range rz;

				rz = Intervals.newRightOpenInterval(
						Integer.valueOf(r1.chr),
						Integer.valueOf(r1.chr + 1));
				re.charset = rz;
				return re;
			}

		});
	}

	/**
	 * 
	 * @param <A>
	 * @param reseq
	 * @return
	 */
	public static<A> NFABuilder<A> newInstance(Inclementor<A> reseq) {
		return new NFABuilder.NBld<A>(reseq);
	}

	/**
	 * 
	 * @param seq
	 * @return
	 */
	public abstract NFAObject<Integer, A, Tuple2<A, Integer>> parse(
			CharSequence seq);
}
