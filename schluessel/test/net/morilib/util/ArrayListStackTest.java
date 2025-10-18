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
package net.morilib.util;

import java.util.Arrays;
import java.util.EmptyStackException;

import net.morilib.lisp.test.TC;
import net.morilib.util.ArrayListStack;


/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2005/10/30
 */
public class ArrayListStackTest extends TC {
	
	/**
	 * 
	 *
	 */
	public void testPush() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		eq(l.push("a"), "a");
		eq(l.list.toArray(), new Object[] { "a" });
		eq(l.push("b"), "b");
		eq(l.list.toArray(), new Object[] { "a", "b" });
	}
	
	/**
	 * 
	 *
	 */
	public void testPop() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		eq(l.pop(), "b");
		eq(l.pop(), "a");
		try {
			l.pop();  fail();
		} catch(EmptyStackException e) {}
	}
	
	/**
	 * 
	 *
	 */
	public void testPeek() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		eq(l.peek(), "b");  l.pop();
		eq(l.peek(), "a");  l.pop();
		try {
			l.peek();  fail();
		} catch(EmptyStackException e) {}
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmpty() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		ok(l.isEmpty());  l.push("a");
		ng(l.isEmpty());  l.pop();
		ok(l.isEmpty());
	}
	
	/**
	 * 
	 *
	 */
	public void testPopi() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");  l.push("c");
		
		try {
			l.pop(4);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		l.pop(2);
		eq(l.peek(), "a");
	}
	
	/**
	 * 
	 *
	 */
	public void testGeti() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		
		try {
			l.get(2);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		eq(l.get(1), "b");
	}
	
	
	public void testToList() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		eq(l.toList(), Arrays.asList(new String[] { "a", "b" }));
	}
	
	
	public void testAdd() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		l.add("c");
		eq(l.toList(), Arrays.asList(new String[] { "a", "b", "c" }));
	}
	
	
	public void testAddAll() {
		ArrayListStack<String> l = new ArrayListStack<String>();
		ArrayListStack<String> m = new ArrayListStack<String>();
		
		l.push("a");  l.push("b");
		m.push("c");  m.push("d");
		l.addAll(m);
		eq(l.toList(), Arrays.asList("a", "b", "c", "d"));
	}

}
