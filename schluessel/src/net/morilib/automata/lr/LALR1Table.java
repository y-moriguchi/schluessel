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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.util.ObjectArray;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/08
 */
public class LALR1Table implements LR1Table {
	
	//
	private LALR1Items goTo;
	
	//
	/*package*/ List<Map<Terminal, Action>>     actionTable;
	/*package*/ List<Map<Nonterminal, Integer>> goToTable;
	/*package*/ List<Conflict> conflicts;
	
	/**
	 * 
	 * @param goTo
	 */
	public LALR1Table(LALR1Items goTo) {
		this.goTo = goTo;
		
		computeTable();
	}
	
	//
	private<T, S> List<Map<T, S>> allocateMap(int size) {
		List<Map<T, S>> res = new ObjectArray<Map<T, S>>(size);
		
		for(int i = 0; i < size; i++) {
			res.set(i, new HashMap<T, S>());
		}
		return res;
	}
	
	//
	/*package*/ void computeTable() {
		ContextFreeGrammar grammar = goTo.getGrammar();
		conflicts = new ArrayList<Conflict>();
		
		// numbering the states
		actionTable = allocateMap(goTo.getSizeOfStates());
		goToTable   = allocateMap(goTo.getSizeOfStates());
		
		//
		for(int i = 0; i < goTo.getSizeOfStates(); i++) {
			Set<LALR1Items.Item>  items  = goTo.getItems(i);
			Map<Terminal, Action> ctable = actionTable.get(i);
			
			// 
			for(LALR1Items.Item item : items) {
				if(item.isReduceState()) {
					//
					ContextFreeRule rule = item.getRule();
					
					if(rule.equals(grammar.getAugmentRule())) {
						// S' -> S*
						ctable.put(
								ContextFreeGrammar.ENDMARKER,
								Action.newAccept());
					} else {
						// reduce
						Set<Terminal> follow = item.getLookaheadSet();
						//System.out.println(item);
						
						for(Terminal k : follow) {
							Action act = (Action)ctable.get(k);
							
							if(act == null) {
								ctable.put(k, Action.newReduce(rule));
							} else if(!act.isShift()) {
								// reduce/reduce conflict
								Conflict cnf = Conflict.newReduceReduce(
										rule,
										act.getReduceRule());
								
								conflicts.add(cnf);
							} else {
								// shift/reduce conflict
								Conflict cnf = Conflict.newShiftReduce(
										k, rule);
								
								conflicts.add(cnf);
							}
						}
					}
				} else {
					GrammarSymbol symbol = item.getDirectedSymbol();
					int nextid = goTo.goToID(i, symbol);
					//System.out.println(item);
					
					if(symbol instanceof Terminal) {
						// shift
						Action act = (Action)ctable.get(symbol);
						
						if(act == null) {
							ctable.put(
									(Terminal)symbol,
									Action.newShift(nextid));
						} else if(!act.isShift()) {
							// shift/reduce conflict
							Conflict cnf = Conflict.newShiftReduce(
									symbol,
									act.getReduceRule());
							
							conflicts.add(cnf);
						}
					} else if(symbol instanceof Nonterminal) {
						// goto
						goToTable.get(i).put(
								(Nonterminal)symbol,
								Integer.valueOf(nextid));
					}
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.usei.grammar.LR1Table#action(int, java.lang.Object)
	 */
	public Action action(int stateID, Terminal terminal) {
		//if(!goTo.getGrammar().isTerminal(terminal)) {
		//	throw new InvalidSymbolException(terminal.toString());
		//}
		return actionTable.get(stateID).get(terminal);
	}

	/* (non-Javadoc)
	 * @see org.usei.grammar.LR1Table#goTo(int, java.lang.Object)
	 */
	public int goTo(int stateID, Nonterminal nonterminal) {
		//if(!goTo.getGrammar().isNonterminal(nonterminal)) {
		//	throw new InvalidSymbolException(nonterminal.toString());
		//}
		return goToTable.get(stateID).get(nonterminal);
	}

	/* (non-Javadoc)
	 * @see org.usei.grammar.LR1Table#getInitialStateID()
	 */
	public int getInitialStateID() {
		return goTo.getInitialStateID();
	}
	
	/* (non-Javadoc)
	 * @see org.usei.grammar.LR1Table#getConflicts()
	 */
	public Collection<Conflict> getConflicts() {
		return Collections.unmodifiableCollection(conflicts);
	}

}
