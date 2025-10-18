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
package net.morilib.lisp;

import java.util.NoSuchElementException;
import java.util.Stack;

// 「深さ数」インデックス
/*package*/ class PatternDepthIndex {
	
	//
	private Stack<Integer> val = new Stack<Integer>();
	
	
	public PatternDepthIndex() {
		// do nothing
	}
	
	
	public PatternDepthIndex(PatternDepthIndex c) {
		val.addAll(c.val);
	}
	
	
	public int depth() {
		return val.size();
	}
	
	
	public PatternDepthIndex addDepthNew() {
		PatternDepthIndex res = new PatternDepthIndex(this);
		
		res.val.push(0);
		return res;
	}
	
	
	public PatternDepthIndex incNew() {
		if(val.isEmpty()) {
			throw new NoSuchElementException();
		}
		
		PatternDepthIndex res = new PatternDepthIndex(this);
		
		int v = res.val.pop();
		v++;
		res.val.push(v);
		return res;
	}
	
	
	public void addDepth() {
		val.push(0);
	}
	
	
	public void inc() {
		if(val.isEmpty()) {
			throw new NoSuchElementException();
		}
		
		int v = val.pop();
		v++;
		val.push(v);
	}
	
	
	public int pop() {
		if(val.isEmpty()) {
			throw new NoSuchElementException();
		}
		
		return val.pop();
	}
	
	
	public int peek() {
		if(val.isEmpty()) {
			throw new NoSuchElementException();
		}
		
		return val.peek();
	}
	
	
	public boolean isAllZero() {
		for(int i = 0; i < val.size(); i++) {
			if(val.get(i) > 0) {
				return false;
			}
		}
		return true;
	}
	
	
	public boolean isTop() {
		return val.isEmpty();
	}
	
	
	public PatternDepthIndex copy() {
		return new PatternDepthIndex(this);
	}
	
	
	public boolean equals(Object o) {
		if(o instanceof PatternDepthIndex) {
			return val.equals(((PatternDepthIndex)o).val);
		}
		return false;
	}
	
	
	public int hashCode() {
		return val.hashCode();
	}
	
	
	public String toString() {
		return val.toString();
	}
	
}
