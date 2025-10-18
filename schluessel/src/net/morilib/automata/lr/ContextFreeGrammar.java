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
package net.morilib.automata.lr;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.Hashes;
import net.morilib.util.Objects;

/**
 * 文脈自由言語を規定し、FIRST集合とFOLLOW集合を計算する。
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/02
 */
public class ContextFreeGrammar {

	/**
	 * FIRST集合を規定する。
	 * 
	 * @author MORIGUCHI, Yuichiro 2008/01/02
	 */
	public static final class First {
		
		//
		private Set<Terminal> first;
		private boolean nullable;
		
		//
		/*package*/ First() {
			this.first = new HashSet<Terminal>();
		}
		
		//
		/*package*/ First(Set<Terminal> first) {
			this.first = first;
		}
		
		//
		/*package*/ First(First f) {
			this.first = new HashSet<Terminal>(f.first);
			this.nullable = f.nullable;
		}
		
		//
		/*package*/ static First singleton(Terminal symbol) {
			return new First(Collections.singleton(symbol));
		}
		
		//
		/*package*/ boolean add(Terminal symbol) {
			return first.add(symbol);
		}
		
		//
		/*package*/ boolean addAll(Set<Terminal> symbols) {
			return first.addAll(symbols);
		}
		
		//
		/*package*/ void setNullable(boolean v) {
			this.nullable = v;
		}
		
		/**
		 * nullでない記号の集合を得る。
		 * 
		 * @return nullでない集合(java.util.Set)
		 */
		public Set<Terminal> getNotNullSymbols() {
			return Collections.unmodifiableSet(first);
		}
		
		/**
		 * 記号が含まれるかを調べる。
		 * 
		 * @param symbol 含まれるかを調べる記号
		 * @return 含まれるときにtrueを得る
		 */
		public boolean contains(Terminal symbol) {
			return first.contains(symbol);
		}
		
		/**
		 * FIRST集合がnullを含むかを調べる。
		 * 
		 * @return nullが含まれるときtrueを得る
		 */
		public boolean isNullable() {
			return nullable;
		}
		
		/**
		 * FIRST集合のサイズを得る。
		 * 
		 * @return 集合のサイズ
		 */
		public int size() {
			return first.size();
		}
		
		/**
		 * このFIRST集合が他のFIRST集合と等しいかを得る。
		 * 
		 * @param o 等しいかを調べるオブジェクト
		 * @return オブジェクトが等しいときにtrueを得る
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object o) {
			if(o instanceof First) {
				First f = (First)o;
				
				return (nullable == f.nullable &&
						Objects.equals(first, f.first));
			}
			return false;
		}
		
		/**
		 * このオブジェクトのハッシュ値を得る。
		 * 
		 * @return ハッシュ値
		 * @see java.lang.Object#hashCode()
		 */
		public int hashCode() {
			int res = Hashes.INIT;
			
			res = Hashes.A * (Hashes.hashCode(first) + res);
			res = Hashes.A * (Hashes.hashCode(nullable) + res);
			return res;
		}
		
		/**
		 * このオブジェクトの文字列表現を得る。
		 * 
		 * @return 文字列表現
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			StringBuffer buf = new StringBuffer("{");
			String d = "";
			
			for(Terminal t : first) {
				buf.append(d);
				buf.append(t);
				d = ", ";
			}
			
			if(nullable) {
				buf.append(d).append("epsilon");
			}
			return buf.append("}").toString();
		}

	}
	
	// ダミーマーカ(LALR(1)表作成時に使用)
	/*package*/ static final Terminal DMY = new Terminal() {
		
		public String toString() {
			return "#";
		}
		
	};

	/**
	 * 終了記号である。
	 */
	public static final Terminal ENDMARKER = new Terminal() {
		
		public String toString() {
			return "endMarker";
		}
		
	};
	
	// 文脈自由言語を規定する要素
	private Set<ContextFreeRule>    rules;    // 文脈自由言語のルール
	//private Set    nonterminals;  // 非終端記号の集合
	//private Set    terminals;    // 終端記号の集合
	private Nonterminal startSymbol;   // 開始記号
	
	// 拡大文脈自由言語のルール
	private final ContextFreeRule augmentRule;
	
	// 拡大文脈自由言語の開始記号
	/*package*/ final Nonterminal augmentSymbol = new Nonterminal() {
		
		public String toString() {
			return "S'";
		}
	};
	
	// result of FIRST(X) and FOLLOW(A)
	private Map<GrammarSymbol, First>    first  =
		new HashMap<GrammarSymbol, First>();
	private Map<Nonterminal,   Set<Terminal>> follow =
		new HashMap<Nonterminal,   Set<Terminal>>();
	
	// 文脈自由言語のクラスを構築する
	/*package*/ ContextFreeGrammar(
			Set<ContextFreeRule>    contextFreeRules,
			//Set    nonterminals,
			//Set    terminals,
			Nonterminal startSymbol) {
		this.rules = new HashSet<ContextFreeRule>(contextFreeRules);
		//this.nonterminals = new HashSet(nonterminals);
		//this.terminals    = new HashSet(terminals);
		this.startSymbol = startSymbol;
		
		// augments the given grammar
		augmentRule = new ContextFreeRule(
				augmentSymbol, new GrammarSymbol[] { startSymbol });
		
		//this.nonterminals.add(augmentSymbol);
		this.rules.add(augmentRule);
		
		//
		//this.terminals.add(ENDMARKER);
		//this.terminals.add(DMY);
	}
	
	/**
	 * 文脈自由言語のインスタンスを生成する。
	 * 
	 * @param contextFreeRules 文脈自由言語の規則のリスト
	 * @param nonterminals 非終端記号の集合
	 * @param terminals 終端記号の集合
	 * @param startSymbol 開始記号
	 * @return 文脈自由言語のインスタンス
	 */
	public static ContextFreeGrammar newInstance(
			Set<ContextFreeRule>   contextFreeRules,
			//Set    nonterminals,
			//Set    terminals,
			Nonterminal startSymbol) {
		// check not null
		if(contextFreeRules  == null) {
			throw new NullPointerException("Null is not allowed");
		}
		
		// check start symbol
		//if(!nonterminals.contains(startSymbol)) {
		//	throw new InvalidSymbolException("Start symbol");
		//}
		
		// construct the object
		ContextFreeGrammar res = new ContextFreeGrammar(
				contextFreeRules,
				//nonterminals,
				//terminals,
				startSymbol);
		
		// compute FIRST and FOLLOW
		res.computeFirst();
		res.computeFollow();
		return res;
	}
	
	/**
	 * 与えられたオブジェクトが終端記号かを調べる。
	 * 
	 * @param symbol 調べるオブジェクト
	 * @return オブジェクトが終端記号のときにtrue
	 */
	/*public boolean isTerminal(Object symbol) {
		return terminals.contains(symbol);
	}*/
	
	/**
	 * 終端記号の集合を得る。
	 * 
	 * @return 終端記号の集合
	 */
	/*public Set getTerminals() {
		return Collections.unmodifiableSet(terminals);
	}*/
	
	/**
	 * 与えられたオブジェクトが非終端記号かを調べる。
	 * 
	 * @param symbol 調べるオブジェクト
	 * @return オブジェクトが非終端記号のときにtrue
	 */
	/*public boolean isNonterminal(Object symbol) {
		return nonterminals.contains(symbol);
	}*/
	
	/**
	 * 非終端記号の集合を得る。
	 * 
	 * @return 非終端記号の集合
	 */
	/*public Set getNonterminals() {
		return Collections.unmodifiableSet(nonterminals);
	}*/
	
	/**
	 * 与えられたオブジェクトが非終端記号または終端記号かを調べる。
	 * 
	 * @param symbol 調べるオブジェクト
	 * @return オブジェクトが非終端記号または終端記号のときにtrue
	 */
	/*public boolean isSymbol(Object symbol) {
		return isTerminal(symbol) || isNonterminal(symbol);
	}*/
	
	/**
	 * 開始記号を得る。
	 * 
	 * @return 開始記号
	 */
	public Nonterminal getStartSymbol() {
		return startSymbol;
	}
	
	/**
	 * 拡大文脈自由言語の規則(S' -&gt; S)を得る。
	 * 
	 * @return 拡大文脈自由言語の規則
	 */
	public ContextFreeRule getAugmentRule() {
		return augmentRule;
	}
	
	/**
	 * 拡大文脈自由言語の記号(S')であるかを調べる。
	 * 
	 * @param symbol 調べるオブジェクト
	 * @return 拡大文脈自由言語の記号のときtrue
	 */
	public boolean isAugmentSymbol(GrammarSymbol symbol) {
		return augmentSymbol.equals(symbol);
	}
	
	/**
	 * 文脈自由言語の規則のリストを得る。
	 * 
	 * @return 文脈自由言語の規則のリスト
	 */
	public Set<ContextFreeRule> getRules() {
		return Collections.unmodifiableSet(rules);
	}

	/**
	 * 文脈自由言語において規則の左辺の非終端記号が
	 * 与えられた非終端記号ではじまるような規則を全て得る。
	 * 
	 * @param nonterminal 探す対象の左辺値
	 * @return 与えられた非終端記号ではじまるような規則
	 */
	public Set<ContextFreeRule> findRules(Nonterminal nonterminal) {
		Set<ContextFreeRule> res = new HashSet<ContextFreeRule>();
		
		//if(!isNonterminal(nonterminal)) {
		//	throw new IllegalArgumentException(
		//			nonterminal + " must be nonterminal");
		//}
		
		for(ContextFreeRule p : rules) {
			if(Objects.equals(p.getLeftSymbol(), nonterminal)) {
				res.add(p);
			}
		}
		return res;
	}
	
	/**
	 * FIRST(与えられた記号)を計算する。
	 * 
	 * @param symbol 文法記号
	 * @return 与えられた文法記号に対応するFIRST集合
	 */
	public First first(GrammarSymbol symbol) {
		if(symbol instanceof Terminal) {
			return First.singleton((Terminal)symbol);
		} else {
			return first.get(symbol);
		}
	}
	
	/**
	 * FIRST(与えられた記号のリスト)を計算する。
	 * 
	 * @param symbol 文法記号のリスト
	 * @return 与えられた文法記号のリストに対応するFIRST集合
	 */
	public First firstAll(List<GrammarSymbol> symbols) {
		First res = new First();
		
		firstSymbols(res, symbols);
		return res;
	}
	
	/*package*/ boolean firstSymbols(
			First res, List<GrammarSymbol> symbols) {
		boolean dirt = false;
		
		Iterator<GrammarSymbol> itr = symbols.iterator();
		while(true) {
			// reached the end of right values, set nullable
			if(!itr.hasNext()) {
				dirt = (res.isNullable() != true);
				res.setNullable(true);
				break;
			}
			
			// get next X_i
			GrammarSymbol y = itr.next();
			
			if(y instanceof Nonterminal) {
				if(first.containsKey(y)) {
					// X_i is nonterminal
					First y2 = (First)first.get(y);
					
					// add FIRST(X_i)
					//System.out.println(y + ":" + y2);
					dirt = res.addAll(y2.getNotNullSymbols()) | dirt;
					
					// break this loop if X_i is not nullable
					if(!y2.isNullable()) {
						break;
					}
				} else {
					// if FIRST(X_i) has not calculated, break this loop
					break;
				}
			} else if(y instanceof Terminal) {
				// X_i is terminal
				dirt = res.add((Terminal)y);
				break;
			} else {
				// invalid symbol
				throw new RuntimeException();
			}
		}
		return dirt;
	}
	
	// computing FIRST(X) function
	/*package*/ boolean computeFirst1() {
		boolean dirt = false;
		
		// iterate for the all found rules
		for(ContextFreeRule rule : rules) {
			List<GrammarSymbol> l = rule.getDerivedSymbols();
			Nonterminal         x = rule.getLeftSymbol();
			First  res = null;
			
			// get FIRST(X)
			res = (First)first.get(x);
			if(res == null) {
				res = new First();
			}
			
			dirt = firstSymbols(res, l) | dirt;
			first.put(x, res);
		}
		return dirt;
	}
	
	//
	/*package*/ void computeFirst() {
		//List lst = new ArrayList(nonterminals);
		first = new HashMap<GrammarSymbol, First>();
		
		while(computeFirst1());
	}
	
	/**
	 * 与えられた非終端記号に対応するFOLLOW集合を得る。
	 * 
	 * @param symbol FOLLOW集合を計算する非終端記号
	 * @return FOLLOW(与えられた非終端記号)
	 */
	public Set<Terminal> follow(Nonterminal symbol) {
		//if(isNonterminal(symbol)) {
		//	return (Set)follow.get(symbol);
		//} else {
		//	throw new IllegalArgumentException();
		//}
		return follow.get(symbol);
	}
	
	//
	/*package*/ static boolean _followPut(
			Map<Nonterminal, Set<Terminal>> f,
			Nonterminal y,
			Set<Terminal> src) {
		Set<Terminal> s = f.get(y);
		
		if(s == null) {
			s = new HashSet<Terminal>();
			f.put(y, s);
		}
		return s.addAll(src);
	}
	
	// computing FOLLOW(A) function
	/*package*/ boolean computeFollow1(
			Set<Terminal> res, Nonterminal lval,
			List<GrammarSymbol> l) {
		boolean dirt = false;
		
		for(int j = 0; j < l.size(); j++) {
			GrammarSymbol y = l.get(j);
			
			if(y instanceof Nonterminal) {
				Nonterminal yy = (Nonterminal)y;
				
				// compute FIRST(beta); beta = X_i+1 .. X_n
				List<GrammarSymbol> subl = l.subList(j + 1, l.size());
				First beta = firstAll(subl);
				
				// add FIRST(X_i+1 .. X_n) to FOLLOW(X_i)
				dirt = _followPut(
						follow, yy, beta.getNotNullSymbols()) | dirt;
				
				// add FOLLOW(A) to FOLLOW(X_i)
				if(beta.isNullable() && follow.containsKey(lval)) {
					// X_0 is nonterminal and already calculated
					//  FOLLOW(lval)
					dirt = _followPut(
							follow, yy, follow.get(lval)) | dirt;
				}
			//} else if(!isTerminal(y)){
			//	// invalid symbol
			//	throw new InvalidSymbolException();
			}
		}
		return dirt;
	}
	
	//
	/*package*/ boolean computeFollow1() {
		Set<Terminal> res  = new HashSet<Terminal>();
		boolean dirt = false;
		
		// iterate for the all rules
		for(ContextFreeRule rule : rules) {
			List<GrammarSymbol> l = rule.getDerivedSymbols();
			Nonterminal lval = rule.getLeftSymbol();
			
			dirt = computeFollow1(res, lval, l) | dirt;
		}
		return dirt;
	}
	
	//
	/*package*/ void computeFollow() {
		//List<GrammarSymbol> lst =
		//	new ArrayList<GrammarSymbol>(nonterminals);
		
		//Collections.reverse(lst);
		follow = new HashMap<Nonterminal, Set<Terminal>>();
		
		// add $ to S
		Set<Terminal> init = new HashSet<Terminal>();
		init.add(ENDMARKER);
		follow.put(augmentSymbol, init);
		
		// 
		while(computeFollow1());
	}

}
