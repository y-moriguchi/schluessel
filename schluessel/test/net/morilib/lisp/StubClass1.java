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

import java.util.List;

public class StubClass1 {
	
	public String field;
	public String[] farray = new String[10];
	
	public StubClass1(int i, long l) {
		System.out.println(i + ":" + l);
	}
	
	public StubClass1(char c, String... a) {
		System.out.println(c);
		for(String s : a) {
			System.out.println(s);
		}
	}
	
	public StubClass1(String s, List<Integer> li) {
		int i = 0;
		
		System.out.println(s);
		for(int e : li) {
			i += e;
		}
		System.out.println(i);
	}
	
	public StubClass1(int[]... is) {
		for(int[] i2 : is) {
			int i = 0;
			
			for(int i1 : i2) {
				i += i1;
			}
			System.out.println(i);
		}
	}
	
	public String getField1() {
		return field;
	}
	
	public void setField1(String val) {
		field = val;
	}
	
	public String[] getFarray() {
		return farray;
	}
	
	public void setFarray(String[] v) {
		farray = v;
	}
	
	public String getFarray(int i) {
		return farray[i];
	}
	
	public void setFarray(int i, String val) {
		farray[i] = val;
	}
	
	public void print(String... a) {
		for(String s : a) {
			System.out.println(s);
			System.out.println(":");
		}
	}
	
	public void excep(int i) {
		throw new RuntimeException("message " + i);
	}
	
}
