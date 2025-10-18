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
package net.morilib.lisp.format;

import java.util.ArrayList;
import java.util.List;

/*package*/ class ArgumentTypeBuf {
	
	
	private static class ArgTypeImpl implements ArgumentType {
		
		private char chararg;
		private int intarg;
		private boolean vararg;
		private boolean defpar = true;
		
		
		public char getChar() {
			return chararg;
		}

		public int getInt() {
			return intarg;
		}

		public boolean isVararg() {
			return vararg;
		}

		public boolean isDefault() {
			return defpar;
		}
		
	}
	
	
	private List<ArgumentType> lst = new ArrayList<ArgumentType>();
	private ArgTypeImpl now = new ArgTypeImpl();
	private boolean atmark;
	private boolean colon;
	
	
	/*package*/ void setChar(char c) {
		now.defpar = false;
		now.chararg = c;
	}
	
	
	/*package*/ void setInt(int i) {
		now.defpar = false;
		now.intarg = i;
	}
	
	
	/*package*/ void setVararg(boolean flg) {
		now.defpar = false;
		now.vararg = flg;
	}
	
	
	/*package*/ void setAtmark(boolean flg) {
		atmark = flg;
	}
	
	
	/*package*/ void setColon(boolean flg) {
		colon = flg;
	}
	
	
	/*package*/ boolean isAtmark() {
		return atmark;
	}
	
	
	/*package*/ boolean isColon() {
		return colon;
	}
	
	
	/*package*/ List<ArgumentType> getArgumentTypeList() {
		return lst;
	}
	
	
	/*package*/ void push() {
		lst.add(now);
		now = new ArgTypeImpl();
	}
	
	
	/*package*/ void refresh() {
		lst = new ArrayList<ArgumentType>();
	}

}
