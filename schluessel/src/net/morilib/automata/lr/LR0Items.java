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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.Hashes;
import net.morilib.util.LinkedListStack;
import net.morilib.util.Objects;
import net.morilib.util.Stack2;
import net.morilib.util.StateGraph;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/01
 */
public final class LR0Items {

	/**
	 * 
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2006/06/25
	 */
	public static final class Item {
		
		//
		private ContextFreeRule rule;
		private int  itemId = 0;
		
		//
		/*package*/ Item(ContextFreeRule rule) {
			if(rule == null) {
				throw new NullPointerException();
			}
			this.rule = rule;
		}
		
		//
		/*package*/ Item(ContextFreeRule rule, int itemId) {
			this(rule);
			this.itemId = itemId;
		}
		
		//
		/*package*/ int getItemId() {
			return itemId;
		}
		
		/**
		 * 
		 * @return
		 */
		public GrammarSymbol getDirectedSymbol() {
			return rule.getDerivedSymbol(itemId);
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isReduceState() {
			return itemId == rule.getDerivedSymbolLength();
		}
		
		/**
		 * 
		 * @return
		 */
		public Item shift() {
			if(!isReduceState()) {
				return new Item(rule, itemId + 1);
			} else {
				throw new IllegalStateException();
			}
		}
		
		/**
		 * 
		 * @return
		 */
		public ContextFreeRule getRule() {
			return rule;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object o) {
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
			r += Hashes.A * (Hashes.hashCode(rule) + r);
			r += Hashes.A * (Hashes.hashCode(itemId) + r);
			
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
			return buf.toString();
		}
		
	}
	
	//
	private ContextFreeGrammar grammar;
	
	// result of GOTO graph
	//private Map<Set<Item>, Map<Object, Set<Item>>> goTo =
	//	new HashMap();
	private StateGraph<Set<Item>, GrammarSymbol> goTo =
		new StateGraph<Set<Item>, GrammarSymbol>();
	private Set<Item> initialItems;
	private Item initialItem;
	
	/**
	 * 
	 * @param grammar
	 */
	/*package*/ LR0Items(ContextFreeGrammar grammar) {
		this.grammar = grammar;
	}
	
	/**
	 * 
	 * @param grammar
	 * @return
	 */
	public static LR0Items build(ContextFreeGrammar grammar) {
		LR0Items res = new LR0Items(grammar);
		
		res.computeGoTo();
		return res;
	}
	
	/**
	 * 
	 * @return
	 */
	public ContextFreeGrammar getGrammar() {
		return grammar;
	}
	
	// extract goto edges from the given items
	/*package*/ static Map<GrammarSymbol, Set<Item>> extractGoToState(
			Set<Item> items) {
		Map<GrammarSymbol, Set<Item>> res =
			new HashMap<GrammarSymbol, Set<Item>>();
		
		for(Item itm : items) {
			if(!itm.isReduceState()) {
				// add the item [A -> alpha X * beta]
				Set<Item> s = res.get(itm.getDirectedSymbol());
				
				if(s == null) {
					s = new HashSet<Item>();
					res.put(itm.getDirectedSymbol(), s);
				}
				s.add(itm.shift());
			}
		}
		return res;
	}
	
	//
	/*package*/ void computeGoTo() {
		Stack2<Set<Item>> stk = new LinkedListStack<Set<Item>>();
		
		initialItem  = new Item(grammar.getAugmentRule());
		initialItems = computeItemClosure(
				Collections.singleton(initialItem));
		stk.push(initialItems);
		
		do {
			// pop a state
			Set<Item> state = stk.pop();
			
			// extract goto edges
			Map<GrammarSymbol, Set<Item>> res =
				extractGoToState(state);
			
			// extract closure of the edges
			for(Map.Entry<GrammarSymbol, Set<Item>> e : res.entrySet()) {
				Set<Item> nval = computeItemClosure(e.getValue());
				
				// found a new state
				if(!goTo.isState(nval)) {
					stk.push(nval);
				}
				
				// set the closure of e.getValue()
				e.setValue(nval);
			}
			
			// add new state and edges
			goTo.addEdgeMap(state, res);
		} while(!stk.isEmpty());
	}
	
	//
	/*package*/ StateGraph<Set<Item>, GrammarSymbol> getGoToMap() {
		return StateGraph.unmodifiableGraph(goTo);
	}
	
	//
	/*package*/ Set<Item> computeItemClosure(Set<Item> items) {
		Stack2<Item> stk = new LinkedListStack<Item>(items);
		Set<Item>    res = new HashSet<Item>(items);
		
		do {
			Item   itm = (Item)stk.pop();
			
			if(!itm.isReduceState() &&
					itm.getDirectedSymbol() instanceof Nonterminal) {
				Set<ContextFreeRule> rules = grammar.findRules(
						(Nonterminal)itm.getDirectedSymbol());
				
				for(ContextFreeRule r : rules) {
					Item i2 = new Item(r);
					
					if(!res.contains(i2)) {
						res.add(i2);
						stk.push(i2);
					}
				}
			}
		} while(!stk.isEmpty());
		
		return res;
	}
	
	/**
	 * 
	 * @param state
	 * @param symbol
	 * @return
	 */
	public Set<Item> goTo(Set<Item> state, GrammarSymbol symbol) {
		if(state == null) {
			throw new NullPointerException("state is null");
		}
		
		return Collections.unmodifiableSet(goTo.get(state, symbol));
	}
	
	/**
	 * 
	 * @param state
	 * @return
	 */
	/*public Map<GrammarSymbol, Set<Item>> getEdges(Set<Item> state) {
		if(state == null) {
			throw new NullPointerException("state is null");
		}
		
		return Collections.unmodifiableMap(goTo.getEdgeMap(state));
	}*/
	
	/**
	 * 
	 * @param state
	 * @return
	 */
	public boolean isInitialState(Set<Item> state) {
		return state.contains(initialItem);
	}
	
	/**
	 * 
	 * @return
	 */
	public Set<Item> getInitialState() {
		return Collections.unmodifiableSet(initialItems);
	}
	
	/**
	 * 
	 * @param item
	 * @return
	 */
	public boolean isInitialItem(Item item) {
		return initialItem.equals(item);
	}
	
	/**
	 * 
	 * @return
	 */
	/*package*/ Collection<Set<Item>> getAllStates() {
		return Collections.unmodifiableCollection(goTo.getAllNodes());
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isKernel(Item item) {
		return (item.itemId > 0 ||
				grammar.isAugmentSymbol(item.rule.getLeftSymbol()));
	}
	
	/**
	 * 
	 * @return
	 */
	public int getStateSize() {
		return goTo.stateSize();
	}

}
