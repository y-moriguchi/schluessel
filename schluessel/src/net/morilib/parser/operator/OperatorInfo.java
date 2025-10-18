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

import net.morilib.util.IntMath;

public abstract class OperatorInfo<T> {
	
	
	public static final class Associative {
		
		//
		private int assoc;
		
		private Associative(int assoc) {
			this.assoc = assoc;
		}
		
		public boolean isEqualTo(Associative a) {
			return assoc == a.assoc;
		}
		
	}
	
	
	public static final Associative LEFT = new Associative(1);
	
	
	public static final Associative EQUAL = new Associative(0);
	
	
	public static final Associative RIGHT = new Associative(-1);
	
	//
	/*package*/ int precedenceF, precedenceG;
	private Associative associative;
	private boolean nextUnary;
	
	
	private static final OperatorInfo<Object>
	BEGIN = new OperatorInfo<Object>(Integer.MIN_VALUE, EQUAL, true) {
		
		public String toString() {
			return "c";
		}

		@Override
		public Object reduce(Object s, OperatorStack<Object> stack) {
			throw new UnsupportedOperationException();
		}

		@Override
		public int howManyTerms() {
			return 0;
		}
		
	};
	
	
	private static final OperatorInfo<Object>
	END = new OperatorInfo<Object>(Integer.MIN_VALUE, EQUAL, false) {
		
		public String toString() {
			return "$";
		}

		@Override
		public Object reduce(Object s, OperatorStack<Object> stack) {
			throw new UnsupportedOperationException();
		}

		@Override
		public int howManyTerms() {
			return 0;
		}
		
	};
	
	
	protected OperatorInfo(
			int precedenceF, int precedenceG,
			Associative associative, boolean nextUnary) {
		this.precedenceF = precedenceF;
		this.precedenceG = precedenceG;
		this.associative = associative;
		this.nextUnary   = nextUnary;
	}
	
	
	protected OperatorInfo(
			int precedence,
			Associative associative, boolean nextUnary) {
		this(precedence, precedence, associative, nextUnary);
	}
	
	
	@SuppressWarnings("unchecked")
	public static final<T> OperatorInfo<T> getBegin() {
		return (OperatorInfo<T>)BEGIN;
	}
	
	
	@SuppressWarnings("unchecked")
	public static final<T> OperatorInfo<T> getEnd() {
		return (OperatorInfo<T>)END;
	}

	/**
	 * 
	 */
	public void shift() {
	}
	
	/**
	 * 
	 * @param s
	 * @param stack
	 * @return
	 */
	public abstract T reduce(Object s, OperatorStack<T> stack);
	
	/**
	 * 
	 * @return
	 */
	public abstract int howManyTerms();
	
	/**
	 * 
	 * @return
	 */
	public boolean canNextUnary() {
		return nextUnary;
	}
	
	//
	/*package*/ int comparePrecedence(OperatorInfo<T> op) {
		int r = IntMath.compareTo(precedenceF, op.precedenceG);
		
		if(r == 0) {
			if(associative == null) {
				throw new NullPointerException();
			}
			return associative.assoc;
		} else {
			return r;
		}
//		return (r == 0) ? associative.assoc : r;
	}
	
	//
	/*package*/ boolean isAssociative(OperatorInfo<T> op) {
		int r = IntMath.compareTo(precedenceF, op.precedenceG);
		
		return (r != 0) || (associative != null);
	}
	
}