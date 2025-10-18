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

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/09
 */
public interface LexicalAnalyser<T> {
	
	/**
	 * 
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2006/07/09
	 */
	public static final class Token<T> {
		
		/**
		 * 
		 */
		protected Terminal grammarSymbol;
		
		/**
		 * 
		 */
		protected T attribute;
		
		//
		private static Token<Object> ENDMARKER =
			new Token<Object>(ContextFreeGrammar.ENDMARKER, null);
		
		/**
		 * 
		 * @param grammarSymbol
		 * @param attribute
		 */
		public Token(Terminal grammarSymbol, T attribute) {
			this.grammarSymbol = grammarSymbol;
			this.attribute     = attribute;
		}
		
		/**
		 * 
		 * @return
		 */
		public Terminal getGrammarSymbol() {
			return grammarSymbol;
		}
		
		/**
		 * 
		 * @return
		 */
		public T getAttribute() {
			return attribute;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return grammarSymbol + ":" + attribute;
		}
		
		
		@SuppressWarnings("unchecked")
		public static<T> Token<T> endMarker() {
			return (Token<T>)ENDMARKER;
		}
		
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isEnded();
	
	/**
	 * 
	 * @return
	 */
	public Token<T> nextToken();

}
