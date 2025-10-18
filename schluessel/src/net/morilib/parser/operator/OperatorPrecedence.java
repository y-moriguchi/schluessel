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
package net.morilib.parser.operator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lang.cont.CPS;
import net.morilib.lang.cont.Continuatable;
import net.morilib.lang.cont.Continuations;
import net.morilib.util.Tokenizer;

public abstract class OperatorPrecedence<T> {
	
	//
	private static class _Stk<T> extends ArrayList<T> {

		//
		private static final long serialVersionUID = 1150887886431943127L;
		
		private void pop(int p) {
			for(int i = 0; i < p; i++) {
				remove(size() - 1);
			}
		}
		
	}
	
	/**
	 * Internal use.
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/09
	 */
	public class Shift {
		
		/**
		 * Internal use.
		 *
		 *
		 * @author MORIGUCHI, Yuichiro 2010/10/09
		 */
		public class Reduce {
			
			//
			private final Object bs;
			private final OperatorInfo<T> cop;
			private final Object cont;
			
			//
			private Reduce(
					Object bs, OperatorInfo<T> cop, Object cont) {
				this.bs   = bs;
				this.cop  = cop;
				this.cont = cont;
			}
			
			@Continuatable
			public Object execute(
					Object s, Object v, OperatorInfo<T> oq) {
				if(stack.size() < oq.howManyTerms()) {
					return null;
				}
				
				T tr1;
				tr1 = oq.reduce(v, getStk(oq.howManyTerms()));
				if(tr1 == null) {
					return null;
				}
				return new CPS(Shift.this,
						s, bs, cop, tr1,
						oq.howManyTerms(), cont);
			}
			
		};
		
		//
		private Tokenizer<Object> tok;
//		private FollowableToken fol;
		private _Stk<T> stack = new _Stk<T>();
		
		private class _Stk2 implements OperatorStack<T> {
			
			private int base;
			
			private _Stk2(int base) {
				this.base = base;
			}
			
			public T refer(int i) {
				return stack.get(i + stack.size() - base);
			}
			
		}
		
		private final OperatorStack<T> stk0 = new _Stk2(0);
		private final OperatorStack<T> stk1 = new _Stk2(1);
		private final OperatorStack<T> stk2 = new _Stk2(2);
		
		//
		private Shift(Tokenizer<Object> tok) {
			this.tok = tok;
//			this.fol = fol;
		}
		
		//
		private OperatorStack<T> getStk(int i) {
			switch(i) {
			case 0: return stk0;
			case 1: return stk1;
			case 2: return stk2;
			default: return new _Stk2(i);
			}
		}
		
		@Continuatable
		public Object execute(
				Object s,
				Object bs,
				OperatorInfo<T> cop,
				T trm,
				int remstk,
				Object cont) {
			OperatorInfo<T> op = toOperator0(
					s, (remstk < 0) && cop.canNextUnary());
			
			// lexer error?
			if(op == null) {
				return null;
			} else if(remstk >= 0) {
				stack.pop(remstk);
				stack.add(trm);
			}
//			System.out.println(cop + ":" + op + ":" + s + ":" + stack);
			
			// is end?
			if(cop.equals(OperatorInfo.getBegin()) &&
					op.equals(OperatorInfo.getEnd())) {
//				return fol.follow(s) ? trm : null;
				return trm;
			}
			
			if(!validateOperator(cop, op)) {
				return null;
			} else if(!cop.isAssociative(op)) {
				return null;
			} else if(cop.comparePrecedence(op) >= 0) {
				return new CPS(cont, s, bs, cop);
			} else {
				cop.shift();
				return new CPS(Shift.this,
						tok.getToken(), s, op, null, -1,
						new Reduce(bs, cop, cont));
			}
			
		}
		
	};
	
	//
	private Map<Object, OperatorInfo<T>> operators2 =
		new HashMap<Object, OperatorInfo<T>>();
	private Map<Object, OperatorInfo<T>> operators1 =
		new HashMap<Object, OperatorInfo<T>>();
	
	
	private OperatorInfo<T> toOperator0(Object t, boolean prefix) {
		OperatorInfo<T> o = (prefix ? operators1 : operators2).get(t);
		
		if(o == null) {
			return toOperator(t);
		}
		return o;
	}
	
	
	protected abstract OperatorInfo<T> toOperator(Object t);
	
	
	protected abstract boolean validateOperator(
			OperatorInfo<T> op1, OperatorInfo<T> op2);
	
	
	public void addOperator(Object op, OperatorInfo<T> info) {
		operators2.put(op, info);
	}
	
	
	public void addPrefix(Object op, OperatorInfo<T> info) {
		operators1.put(op, info);
	}
	
	
	public Object parse(Tokenizer<Object> o) {
		return CPS.invoke(
				new Shift(o), false,
				o.getToken(), null,
				OperatorInfo.getBegin(), null, -1,
				Continuations.K);
	}
	
}
