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
package net.morilib.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import net.morilib.parser.operator.OperatorInfo;
import net.morilib.parser.operator.OperatorPrecedence;
import net.morilib.parser.operator.OperatorStack;
import net.morilib.util.mapset.IndexedManyToManySet;
import net.morilib.util.mapset.ManyToManySet;
import net.morilib.util.set.Flat3CachedHashSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/09
 */
public final class NewPropertiesParser {
	
	//
	private enum Token { COMMA, EQUAL };
	
	//
	private final Set<String> kset = new Flat3CachedHashSet<String>();
	private final Set<String> vset = new Flat3CachedHashSet<String>();
	private Set<String> nset = kset;
	private ManyToManySet<String, String> mset =
		new IndexedManyToManySet<String, String>();
	
	//
	/*package*/ final OperatorInfo<Object>
	elem = new OperatorInfo<Object>(1000, null, false) {

		@Override
		public Object reduce(Object o, OperatorStack<Object> s) {
			String pt = (String)o;
			
			nset.add(pt);
			return pt;
		}

		@Override
		public int howManyTerms() {
			return 0;
		}

		public String toString() {
			return "var";
		}
		
	};
	
	//
	/*package*/ final OperatorInfo<Object>
	comma = new OperatorInfo<Object>(800, OperatorInfo.LEFT, false) {

		@Override
		public Object reduce(Object o, OperatorStack<Object> s) {
			return ",";
		}

		@Override
		public int howManyTerms() {
			return 2;
		}

		public String toString() {
			return "var";
		}
		
	};
	
	//
	/*package*/ final OperatorInfo<Object>
	equal = new OperatorInfo<Object>(500, null, false) {

		@Override
		public void shift() {
			nset = vset;
		}
		
		@Override
		public Object reduce(Object o, OperatorStack<Object> s) {
			return "=";
		}

		@Override
		public int howManyTerms() {
			return 2;
		}

		public String toString() {
			return "var";
		}
		
	};
	
	//
	/*package*/ final OperatorPrecedence<Object>
	defaultPrc = new OperatorPrecedence<Object>() {
		
		@Override
		protected OperatorInfo<Object> toOperator(Object t) {
			if(t instanceof String) {
				return elem;
			} else {
				return null;
			}
		}

		@Override
		protected boolean validateOperator(
				OperatorInfo<Object> op1,
				OperatorInfo<Object> op2) {
			return true;
		}
		
	};
	
	/*initialize*/ {
		defaultPrc.addOperator(Token.COMMA, comma);
		defaultPrc.addOperator(Token.EQUAL, equal);
		defaultPrc.addOperator(
				QuoteLexer.END,
				OperatorInfo.<Object>getEnd());
	}
	
	//
	private QuoteLexer newPropertiesLexer() {
		Map<Character, Token> m = new HashMap<Character, Token>();
		
		m.put(',', Token.COMMA);
		m.put('=', Token.EQUAL);
		return new QuoteLexer(m);
	}
	
	//
	public ManyToManySet<String, String> parse(
			InputStream ins) throws IOException {
		BufferedReader rd;

		try {
			rd = new BufferedReader(ParserUtils.readEncoding(ins));
		} catch(ParsingException e) {
			return mset;
		}

		// read lines
		QuoteLexer lexer = newPropertiesLexer();
		String line;
		while((line = rd.readLine()) != null) {
			if(line.isEmpty() || line.charAt(0) == '#') {
				continue;
			}
			lexer.setString(line);
			if(defaultPrc.parse(lexer) == null) {
				throw new ParsingException();
			}
			mset.put(kset, vset);
			kset.clear();
			vset.clear();
			nset = kset;
		}
		return mset;
	}
	
}
