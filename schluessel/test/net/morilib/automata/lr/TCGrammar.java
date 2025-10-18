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

import net.morilib.automata.lr.Nonterminal;
import net.morilib.automata.lr.Terminal;
import net.morilib.lisp.test.TC;

public class TCGrammar extends TC {

	static class GST implements Terminal {
		
		String sym;
		
		public GST(String s) {
			sym = s;
		}
		
		public String toString() {
			return sym;
		}
		
	}
	
	static class GSN implements Nonterminal {
		
		String sym;
		
		public GSN(String s) {
			sym = s;
		}
		
		public String toString() {
			return sym;
		}
		
	}
	
}
