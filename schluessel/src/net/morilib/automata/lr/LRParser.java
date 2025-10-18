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

import java.util.HashMap;
import java.util.Map;

import net.morilib.util.ArrayListStack;
import net.morilib.util.LinkedListStack;
import net.morilib.util.Stack2;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/09
 */
public class LRParser<T> {
	
	//
	private static final class Strct {
		
		//
		private int id;
		private GrammarSymbol symbol;
		
		//
		private Strct(int id, GrammarSymbol symbol) {
			this.id = id;
			this.symbol = symbol;
		}
		
		//
		public String toString() {
			return "(" + id + ":" + symbol + ")";
		}
		
	}
	
	//
	private static class SStack<T> implements SemanticAttributes<T> {
		
		//
		private Stack2<T> stack;
		private ContextFreeRule rule;
		
		//
		private SStack(ContextFreeRule rule, Stack2<T> stack) {
			this.rule  = rule;
			this.stack = stack;
		}

		//
		public T get(int index) {
			if(index >= rule.getDerivedSymbolLength()) {
				throw new IndexOutOfBoundsException();
			}
			
			//return stack.get(
			//		rule.getDerivedSymbolLength() - index - 1);
			return stack.get(stack.size() - 
					rule.getDerivedSymbolLength() + index);
		}
		
	}
	
	//
	private LR1Table table;
	private Map<ContextFreeRule, ContextFreeReduceAction<T>> actionMap =
		new HashMap<ContextFreeRule, ContextFreeReduceAction<T>>();
	
	/**
	 * 
	 * @param table
	 */
	public LRParser(LR1Table table) {
		this.table = table;
	}
	
	/**
	 * 
	 * @param rule
	 * @return
	 */
	public ContextFreeReduceAction<T> getAction(
			ContextFreeRule rule) {
		return (ContextFreeReduceAction<T>)actionMap.get(rule);
	}
	
	/**
	 * 
	 * @param rule
	 * @param action
	 */
	public void setAction(
			ContextFreeRule rule,
			ContextFreeReduceAction<T> action) {
		actionMap.put(rule, action);
	}
	
	/**
	 * 
	 * @param rule
	 */
	public void removeAction(ContextFreeRule rule) {
		actionMap.remove(rule);
	}
	
	/**
	 * 
	 * @param lexer
	 */
	public T parse(LexicalAnalyser<T> lexer) throws LRParseException {
		Stack2<Strct> syntax       = new LinkedListStack<Strct>();
		Stack2<T>     semantics    = new ArrayListStack<T>();
		LexicalAnalyser.Token<T> a = lexer.nextToken();
		
		Strct s0 = new Strct(table.getInitialStateID(), null);
		syntax.push(s0);
		
		while(true) {
			Strct s = (Strct)syntax.peek();
			if(s == null) {
				throw new NullPointerException("");
			}
			//System.out.println(syntax);
			
			LR1Table.Action action =
				table.action(s.id, a.getGrammarSymbol());
			
			if(action == null) {
				// error recovery
				throw new LRParseException(a.toString());
			} else if(action.isShift()) {
				syntax.push(new Strct(
						action.getNextStateID(),
						a.getGrammarSymbol()));
				semantics.push(a.getAttribute());
				
				a = lexer.nextToken();
			} else if(action.isReduce()) {
				ContextFreeRule rule = action.getReduceRule();
				
				// execute the action
				T res = null;
				ContextFreeReduceAction<T> ex = getAction(rule);
				if(ex != null) {
					SStack<T> stk = new SStack<T>(rule, semantics);
					
					res = ex.action(rule, stk);
				}
				
				syntax.pop(rule.getDerivedSymbolLength());
				semantics.pop(rule.getDerivedSymbolLength());
				
				Strct ss = (Strct)syntax.peek();
				syntax.push(new Strct(
						table.goTo(ss.id, rule.getLeftSymbol()),
						rule.getLeftSymbol()));
				semantics.push(res);
			} else if(action.isAccept()) {
				return semantics.peek();
			} else {
				throw new IllegalStateException();
			}
		}
	}

}
