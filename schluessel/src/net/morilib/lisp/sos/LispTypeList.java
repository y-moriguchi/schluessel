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
package net.morilib.lisp.sos;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public final class LispTypeList {
	
	//
	private List<LispType> types;
	private boolean rest = false;
	
	
	public LispTypeList(Collection<LispType> c, boolean rest) {
		if(c == null) {
			throw new NullPointerException();
		}
		types = new ArrayList<LispType>(c);
		this.rest = rest;
	}
	
	
	public LispTypeList(boolean rest, LispType... a) {
		if(a == null) {
			throw new NullPointerException();
		}
		types = Arrays.asList((LispType[])a.clone());
		this.rest = rest;
	}
	
	
	public LispTypeList(Collection<LispType> c) {
		this(c, false);
	}
	
	
	public LispTypeList(LispType... a) {
		this(false, a);
	}
	
	
	public LispType get(int index) {
		return types.get(index);
	}
	
	
	public boolean isRest() {
		return rest;
	}
	
	
	public int size() {
		return types.size();
	}
	
	
	public boolean equals(Object o) {
		if(o instanceof LispTypeList) {
			LispTypeList l = (LispTypeList)o;
			
			return types.equals(l.types) && rest == l.rest;
		}
		return false;
	}
	
	
	public int hashCode() {
		return types.hashCode();
	}
	
}
