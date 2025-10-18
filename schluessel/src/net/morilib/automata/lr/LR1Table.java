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

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/08
 */
public interface LR1Table {

	/**
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2006/07/08
	 */
	public static final class Action {
		
		/**
		 * 
		 */
		public static final int SHIFT  = 1;
		
		/**
		 * 
		 */
		public static final int REDUCE = 2;
		
		/**
		 * 
		 */
		public static final int ACCEPT = 3;
		
		//
		private int action;
		private int nextStateID;
		private ContextFreeRule reduceRule;
		
		//
		private Action() {
			// do nothing
		}
		
		/**
		 * 
		 * @param id
		 */
		protected static Action newShift(int id) {
			Action res = new Action();
			
			res.action = SHIFT;
			res.nextStateID = id;
			return res;
		}
		
		/**
		 * 
		 * @param rule
		 */
		protected static Action newReduce(ContextFreeRule rule) {
			Action res = new Action();
			
			res.action = REDUCE;
			res.reduceRule = rule;
			return res;
		}
		
		/**
		 * 
		 * @param id
		 */
		protected static Action newAccept() {
			Action res = new Action();
			
			res.action = ACCEPT;
			return res;
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isShift() {
			return action == SHIFT;
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isReduce() {
			return action == REDUCE;
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isAccept() {
			return action == ACCEPT;
		}
		
		/**
		 * 
		 * @return
		 */
		public int getAction() {
			return action;
		}
		
		/**
		 * 
		 * @return
		 */
		public int getNextStateID() {
			return nextStateID;
		}
		
		/**
		 * 
		 * @return
		 */
		public ContextFreeRule getReduceRule() {
			return reduceRule;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			StringBuffer buf = new StringBuffer();
			
			if(action == SHIFT) {
				buf.append("shift: ").append(nextStateID);
			} else if(action == REDUCE) {
				buf.append("reduce: ").append(reduceRule);
			} else if(action == ACCEPT) {
				buf.append("accept");
			}
			return buf.toString();
		}

	}

	/**
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2006/07/08
	 */
	public static final class Conflict {
		
		//
		private boolean shiftReduce;
		private ContextFreeRule reduceRule, reduceRule2;
		private Object shiftSymbol;
		
		//
		private Conflict() {
			// do nothing
		}
		
		/**
		 * 
		 * @param symbol
		 * @param rule1
		 * @return
		 */
		protected static Conflict newShiftReduce(
				Object symbol, ContextFreeRule rule1) {
			Conflict res = new Conflict();
			
			res.shiftReduce = true;
			res.shiftSymbol = symbol;
			res.reduceRule  = rule1;
			return res;
		}
		
		/**
		 * 
		 * @param symbol
		 * @param rule1
		 * @return
		 */
		protected static Conflict newReduceReduce(
				ContextFreeRule rule1, ContextFreeRule rule2) {
			Conflict res = new Conflict();
			
			res.shiftReduce = false;
			res.reduceRule  = rule1;
			res.reduceRule2 = rule2;
			return res;
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isShiftReduce() {
			return shiftReduce;
		}
		
		/**
		 * 
		 * @return
		 */
		public boolean isReduceReduce() {
			return !shiftReduce;
		}
		
		/**
		 * 
		 * @return
		 */
		public Object getShiftSymbol() {
			return shiftSymbol;
		}
		
		/**
		 * 
		 * @return
		 */
		public ContextFreeRule getReduceRule() {
			return reduceRule;
		}
		
		/**
		 * 
		 * @return
		 */
		public ContextFreeRule getReduceRule2() {
			return reduceRule2;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			StringBuffer buf = new StringBuffer();
			
			if(shiftReduce) {
				buf.append("shift/reduce conflict: shift ");
				buf.append(shiftSymbol).append(" vs reduce ");
				buf.append(reduceRule);
			} else {
				buf.append("reduce/reduce conflict: reduce ");
				buf.append(reduceRule ).append(" vs reduce ");
				buf.append(reduceRule2);
			}
			return buf.toString();
		}
		
	}
	
	/**
	 * 
	 * @param stateID
	 * @param terminal
	 * @return
	 */
	public Action action(int stateID, Terminal terminal);
	
	/**
	 * 
	 * @param stateID
	 * @param nonterminal
	 * @return
	 */
	public int goTo(int stateID, Nonterminal nonterminal);
	
	/**
	 * 
	 * @return
	 */
	public int getInitialStateID();
	
	/**
	 * 
	 * @return
	 */
	public Collection<Conflict> getConflicts();

}
