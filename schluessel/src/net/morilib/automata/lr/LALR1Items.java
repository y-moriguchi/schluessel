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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.Hashes;
import net.morilib.util.ObjectArray;
import net.morilib.util.StateGraph;
import net.morilib.util.LinkedListStack;
import net.morilib.util.Objects;
import net.morilib.util.Stack2;

/**
 * LALR(1)文法のアイテムを格納するクラスである。
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/09
 */
public final class LALR1Items {

	/**
	 * LALR(1)におけるLALR(0)アイテムである。
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2006/06/25
	 */
	public static final class Item {
		
		//
		private ContextFreeRule rule;
		private Set<Terminal> lookahead;
		
		//
		private int  itemId = 0;
		
		//
		/*package*/ Item(ContextFreeRule rule) {
			this(rule, new HashSet<Terminal>());
		}
		
		//
		/*package*/ Item(
				ContextFreeRule rule, Set<Terminal> lookahead) {
			if(rule == null) {
				throw new NullPointerException();
			}
			this.rule = rule;
			this.lookahead = lookahead;
		}
		
		//
		/*package*/ Item(
				ContextFreeRule rule,
				Set<Terminal> lookahead, int itemId) {
			this(rule, lookahead);
			this.itemId = itemId;
		}
		
		//
		/*package*/ Item(Item item) {
			this(item.rule,
					new HashSet<Terminal>(item.lookahead),
					item.itemId);
		}
		
		//
		/*package*/ Item(LR0Items.Item item0) {
			this(item0.getRule(),
					new HashSet<Terminal>(), item0.getItemId());
		}
		
		//
		/*package*/ List<GrammarSymbol> getRightOfDirectedSymbol() {
			List<GrammarSymbol> lst = rule.getDerivedSymbols();
			
			return new LinkedList<GrammarSymbol>(
					lst.subList(itemId, lst.size()));
		}
		
		//
		/*package*/ int getItemId() {
			return itemId;
		}
		
		/**
		 * アイテムの印のついた文法記号を取得する。
		 * 
		 * @return 印のついた文法記号
		 */
		public GrammarSymbol getDirectedSymbol() {
			return rule.getDerivedSymbol(itemId);
		}
		
		/**
		 * アイテムが還元状態であるかを取得する。
		 * 
		 * @return 還元状態のときtrue
		 */
		public boolean isReduceState() {
			return itemId == rule.getDerivedSymbolLength();
		}
		
		/**
		 * 先読み状態の集合を得る。
		 * 
		 * @return 先読み状態の集合
		 */
		public Set<Terminal> getLookaheadSet() {
			return Collections.unmodifiableSet(lookahead);
		}
		
		/**
		 * アイテムの印をシフトする。
		 * 
		 * @return シフトされたアイテム
		 */
		public Item shift() {
			if(!isReduceState()) {
				return new Item(rule, lookahead, itemId + 1);
			} else {
				throw new IllegalStateException();
			}
		}
		
		/**
		 * アイテムの元となる規則を得る。
		 * 
		 * @return アイテムの元となる規則
		 */
		public ContextFreeRule getRule() {
			return rule;
		}
		
		/**
		 * 右辺値の記号列が存在しない(epsilon遷移のとき)trueを得る。
		 * 
		 * @return 右辺値の記号列が存在しない(epsilon遷移のとき)true
		 */
		public boolean isEpsilon() {
			return rule.isEpsilon();
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object o) {
			if(o instanceof Item) {
				Item item = (Item)o;
				
				return (Objects.equals(rule, item.rule) &&
						Objects.equals(lookahead, item.lookahead) &&
						itemId == item.itemId);
			}
			return false;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equalsWithoutLookahead(Object o) {
			if(o instanceof Item) {
				Item item = (Item)o;
				
				return (Objects.equals(rule, item.rule) &&
						itemId == item.itemId);
			}
			return false;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		public int hashCode() {
			int r = Hashes.INIT;
			r += Hashes.A * Hashes.hashCode(rule) + r;
			r += Hashes.A * Hashes.hashCode(itemId) + r;
			r += Hashes.A * Hashes.hashCode(lookahead) + r;
			
			return r;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			List<GrammarSymbol> derived = rule.getDerivedSymbols();
			StringBuffer buf = new StringBuffer();
			
			buf.append(Objects.toString(rule.getLeftSymbol()));
			buf.append(" -> ");
			for(int i = 0; i < derived.size(); i++) {
				if(i == itemId) {
					buf.append("*");
				}
				buf.append(derived.get(i));
				buf.append(" ");
			}
			buf.append(", ").append(lookahead);
			return buf.toString();
		}
		
	}
	
	//
	private ContextFreeGrammar grammar;
	
	// result of GOTO graph
	// goToのキーとなるLR(1)集は、itemsArrayを通じて間接的に保有する
	// (キー項目を変更するため)
	//private Map<Integer, Map<GrammarSymbol, Integer>> goTo =
	//	new HashMap<Integer, Map<GrammarSymbol, Integer>>();
	private StateGraph<Integer, GrammarSymbol> goTo =
		new StateGraph<Integer, GrammarSymbol>();
	private List<Set<Item>> itemsArray;
	private int   initialItems;
	
	//
	/*package*/ LALR1Items(ContextFreeGrammar g) {
		grammar = g;
	}
	
	/**
	 * 
	 * @param lr0
	 * @return
	 */
	public static LALR1Items newLALR(ContextFreeGrammar g) {
		LR0Items   lr0 = LR0Items.build(g);;
		LALR1Items res = new LALR1Items(g);
		
		res.initializeItems(lr0);
		
		boolean dirt = res.spontaneousLookahead();
		do {
			/*for(int i = 0; i < res.itemsArray.length; i++) {
				System.out.println("dd:" + res.itemsArray[i]);
			}
			System.out.println();*/
			dirt = res.propagateLookahead();
			dirt = res.spontaneousLookahead() | dirt;
		} while(dirt);
		
		return res;
	}
	
	//
	/*package*/ boolean addLookahead(
			Set<Item> items, Set<Terminal> lookaheads) {
		boolean res = false;
		
		for(Item i : items) {
			res = i.lookahead.addAll(lookaheads) | res;
		}
		return res;
	}
	
	private boolean propagateLookaheadItem(
			int i, Item j, Item k, boolean dirt) {
		boolean res = dirt;
		
		if(!k.isReduceState()) {
			// get items by indirect reference
			GrammarSymbol xx = k.getDirectedSymbol();
			Set<Item> gg = findGoTo(goTo, i, xx);
			
			if(gg != null &&
				k.lookahead.contains(ContextFreeGrammar.DMY)) {
				// propagate the lookahead(old)
				//res = addLookahead(gg, j.lookahead) | res;
				
				// propagate the lookahead
				for(Item l : gg) {
					//System.out.println("ll:" + l);
					
					//Set lhb = new HashSet(k.lookahead);
					//lhb.remove(ContextFreeGrammar.DMY);
					if(l.equalsWithoutLookahead(k.shift())) {
						res = l.lookahead.addAll(j.lookahead) | res;
					}
				}
			}
		} else if(k.isEpsilon()) {
			Item kk = findItem(goTo, i, k);
			
			res = kk.lookahead.addAll(j.lookahead) | res;
		}
		return res;
	}
	
	//
	/*package*/ boolean propagateLookahead() {
		boolean res = false;
		Set<Integer> items = goTo.getAllNodes();
		
		for(Integer i : items) {
			// each item B -> gamma * delta
			for(Item j : itemsArray.get(i)) {
				//System.out.println("it: " + j);
				
				// ignore if the item is not kernel
				if(isKernel(j)) {
					// compute closure of [B -> g * d, #]
					Item jx = new Item(
							j.getRule(),
							new HashSet<Terminal>(),
							j.itemId);
					
					jx.lookahead.add(ContextFreeGrammar.DMY);
					
					Set<Item> cl = computeItemClosure(
							Collections.singleton(jx));
					//System.out.println("jx: " + jx);
					//System.out.println("cl: " + cl);
					
					// each item of the closure [A -> a * X b, a]
					for(Item k : cl) {
						//System.out.println("kk:" + k);
						res = propagateLookaheadItem(i, j, k, res);
					}
				}
			}
		}
		return res;
	}
	
	private boolean spontaneousLookaheadItem(
			int i, Item j, Item k, boolean dirt) {
		boolean res = dirt;
		
		if(!k.isReduceState()) {
			// get items by indirect reference
			GrammarSymbol xx = k.getDirectedSymbol();
			Set<Item> gg = findGoTo(goTo, i, xx);
			//System.out.println(gg);
			
			if(gg != null) {
				// add the lookahead spontaneously generated
				for(Item l : gg) {
					//System.out.println("ll:" + l);
					if(l.equalsWithoutLookahead(k.shift())) {
						Set<Terminal> lhb =
							new HashSet<Terminal>(k.lookahead);
						
						lhb.remove(ContextFreeGrammar.DMY);
						res = l.lookahead.addAll(lhb) | res;
					}
				}
			}
		} else if(k.isEpsilon()) {
			Item kk = findItem(goTo, i, k);
			Set<Terminal> lhb = new HashSet<Terminal>(k.lookahead);
			
			lhb.remove(ContextFreeGrammar.DMY);
			res = kk.lookahead.addAll(lhb) | res;
		}
		return res;
	}
	
	//
	/*package*/ boolean spontaneousLookahead() {
		boolean res = false;
		Set<Integer> items = goTo.getAllNodes();
		
		for(Integer i : items) {
			// each item B -> gamma * delta
			for(Item j : itemsArray.get(i)) {
				//System.out.println("it: " + j);
				
				// ignore if the item is not kernel
				if(isKernel(j)) {
					// compute closure of [B -> g * d, #]
					Item jx = new Item(
							j.getRule(),
							new HashSet<Terminal>(),
							j.itemId);
					jx.lookahead.add(ContextFreeGrammar.DMY);
					
					Set<Item> cl = computeItemClosure(
							Collections.singleton(jx));
					//System.out.println("jx: " + jx);
					//System.out.println("cl: " + cl);
					
					// each item of the closure [A -> a * X b, a]
					for(Item k : cl) {
						//System.out.println("kk:" + k);
						res = spontaneousLookaheadItem(i, j, k, res);
					}
				}
			}
		}
		return res;
	}
	
	//
	/*package*/ void initializeItems(LR0Items lr0) {
		Set<Set<LR0Items.Item>> lr0g = lr0.getGoToMap().getAllNodes();
		StateGraph<Integer, GrammarSymbol> mp =
			new StateGraph<Integer, GrammarSymbol>();
		Map<Set<LR0Items.Item>, Integer> taio =
			new HashMap<Set<LR0Items.Item>, Integer>();
		itemsArray = new ObjectArray<Set<Item>>(lr0.getStateSize());
		
		// extends LR(0) items to LR(1)
		Iterator<Set<LR0Items.Item>> itr = lr0g.iterator();
		
		for(int ci = 0; itr.hasNext(); ci++) {
			Set<LR0Items.Item> s1 = (Set<LR0Items.Item>)itr.next();
			
			// 
			Set<Item> r1 = new HashSet<Item>();
			for(LR0Items.Item j : s1) {
				Item ni = new Item(j);
				
				// add $ to [S' -> S]
				if(lr0.isInitialState(s1)) {
					ni.lookahead.add(ContextFreeGrammar.ENDMARKER);
				}
				r1.add(ni);
			}
			
			itemsArray.set(ci, r1);
			mp.addNode(ci);
			taio.put(s1, ci);
		}
		
		// copy edges of goto graph
		for(Set<LR0Items.Item> s0 : lr0g) {
			// (grammar symbol, items)
			Map<GrammarSymbol, Set<LR0Items.Item>> p0 =
				lr0.getGoToMap().getEdgeMap(s0);
			Integer s1 = (Integer)taio.get(s0);
			//Map<GrammarSymbol, Integer> p1 = mp.getEdgeMap(s1);
			
			//
			for(GrammarSymbol j : p0.keySet()) {
				//p1.put(j, taio.get(p0.get(j)));
				mp.addTrans(s1, j, taio.get(p0.get(j)));
			}
		}
		
		//
		goTo = mp;
		initialItems =
			((Integer)taio.get(lr0.getInitialState())).intValue();
	}
	
	//
	/*package*/ Item findSameRule(ContextFreeRule r, Set<Item> r2) {
		for(Item k : r2) {
			if(r.equals(k.getRule())) {
				return k;
			}
		}
		return null;
	}
	
	//
	/*package*/ Set<Item> computeItemClosure(Set<Item> items) {
		Stack2<Item> stk = new LinkedListStack<Item>(items);
		Set<Item>    res = new HashSet<Item>(items);
		
		do {
			// production [A -> alpha *B beta, a]
			Item   itm  = (Item)stk.pop();
			
			// extract beta
			List<GrammarSymbol> beta = itm.getRightOfDirectedSymbol();
			GrammarSymbol bb   = null;
			if(!beta.isEmpty()) {
				bb = beta.remove(0);
			}
			//System.out.println(itm + ":" + beta);
			
			if(!itm.isReduceState() && bb instanceof Nonterminal) {
				Set<ContextFreeRule> rules =
					grammar.findRules((Nonterminal)bb);
				//System.out.println("rules:" + rules);
				
				// each production B -> gamma
				for(ContextFreeRule r : rules) {
					// each b in FIRST(beta + a)
					Set<Terminal> bs =
						new HashSet<Terminal>();
					
					for(Terminal kn : itm.lookahead) {
						ContextFreeGrammar.First f;
						
						// compute FIRST(beta + a)
						List<GrammarSymbol> beta2 =
							new LinkedList<GrammarSymbol>(beta);
						beta2.add(kn);
						f = grammar.firstAll(beta2);
						
						bs.addAll(f.getNotNullSymbols());
					}
					//System.out.println("bs:" + bs);
					
					// find rule
					Item itm2 = findSameRule(r, res);
					if(itm2 == null) {
						itm2 = new Item(r);
					} else {
						// 新しいlookaheadと置き換える
						res.remove(itm2);
					}
					
					// add item
					if(itm2.lookahead.addAll(bs)) {
						stk.push(itm2);
					}
					res.add(itm2);
				}
			}
		} while(!stk.isEmpty());
		
		return res;
	}
	
	//
	/*package*/ StateGraph<Integer, GrammarSymbol> getGoTo() {
		return goTo;
	}
	
	//
	/*package*/ String toStringGoTo() {
		StringBuffer buf = new StringBuffer();
		
		for(int i = 0; i < itemsArray.size(); i++) {
			buf.append(i).append(":").append(itemsArray.get(i));
			buf.append("\n");
		}
		
		return buf.toString();
	}
	
	//
	/*package*/ Set<Item> findGoTo(
			StateGraph<Integer, GrammarSymbol> aGoTo,
			int state,
			GrammarSymbol symbol) {
		return itemsArray.get(aGoTo.get(state, symbol));
	}
	
	//
	/*package*/ Item findItem(
			StateGraph<Integer, GrammarSymbol> aGoTo,
			Integer state,
			Item itm) {
		for(Item i : itemsArray.get(state)) {
			//System.out.println("i1:" + i);
			//System.out.println("it:" + itm);
			if(i.equalsWithoutLookahead(itm)) {
				return i;
			}
		}
		return null;
	}
	
	/**
	 * 
	 * @param stateid
	 * @param symbol
	 * @return
	 */
	public int goToID(int stateid, GrammarSymbol symbol) {
		if(stateid < 0 || stateid >= itemsArray.size()) {
			throw new IndexOutOfBoundsException("" + stateid);
		}
		
		return goTo.get(stateid, symbol);
	}
	
	/**
	 * 
	 * @return
	 */
	public int getInitialStateID() {
		return initialItems;
	}
	
	/**
	 * 
	 * @param stateid
	 * @return
	 */
	public Set<Item> getItems(int stateid) {
		if(stateid < 0 || stateid >= itemsArray.size()) {
			throw new IndexOutOfBoundsException("" + stateid);
		}
		
		return Collections.unmodifiableSet(itemsArray.get(stateid));
	}
	
	/**
	 * 
	 * @return
	 */
	public int getSizeOfStates() {
		return itemsArray.size();
	}
	
	/**
	 * 
	 * @return
	 */
	public ContextFreeGrammar getGrammar() {
		return grammar;
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isKernel(Item item) {
		return (item.itemId > 0 ||
				grammar.isAugmentSymbol(item.rule.getLeftSymbol()));
	}

}
