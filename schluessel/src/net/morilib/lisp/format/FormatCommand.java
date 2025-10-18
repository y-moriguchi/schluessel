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

import java.util.Collections;
import java.util.List;

public abstract class FormatCommand implements FormatAtom {
	
	//
	private List<ArgumentType> parameters;
	private boolean atmark;
	private boolean colon;
	
	
	protected FormatCommand(
			List<ArgumentType> parameters,
			boolean atmark, boolean colon) {
		this.parameters = Collections.unmodifiableList(parameters);
		this.atmark     = atmark;
		this.colon      = colon;
	}
	
	
	protected ArgumentType get(int index) {
		if(index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index >= parameters.size()) {
			return ArgumentType.DEFAULT_ARG;
		} else {
			return parameters.get(index);
		}
	}
	
	
	protected int getInt(
			int index,
			FormatArguments args,
			int deflt) throws LispFormatException {
		ArgumentType a = get(index);
		
		return a.isDefault() ? deflt :
			a.isVararg() ? args.shiftInt() : a.getInt();
	}
	
	
	protected char getChar(
			int index,
			FormatArguments args,
			char deflt) throws LispFormatException {
		ArgumentType a = get(index);
		
		return a.isDefault() ? deflt :
			a.isVararg() ? args.shiftChar() : a.getChar();
	}
	
	
	protected boolean isAtmark() {
		return atmark;
	}
	
	
	protected boolean isColon() {
		return colon;
	}
	
}
