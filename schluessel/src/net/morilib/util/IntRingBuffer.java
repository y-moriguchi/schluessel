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
package net.morilib.util;

import java.util.NoSuchElementException;

import net.morilib.lang.number.intmodulo.IntResidue;

public class IntRingBuffer implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 5015372387740172285L;
	
	//
	private int[] buffer;
	private IntResidue ptrt;
	private IntResidue ptrb;
	
	
	public IntRingBuffer(int size) {
		buffer = new int[size];
		ptrt = ptrb = IntResidue.valueOf(0, size);
	}
	
	
	public boolean add(int c) {
		buffer[ptrt.intValue()] = c;
		
		ptrt = ptrt.inclement();
		if(ptrt.equals(ptrb)) {
			ptrb = ptrb.inclement();
			return false;
		} else {
			return true;
		}
	}
	
	
	public int remove() {
		int res;
		
		if(ptrt.equals(ptrb)) {
			throw new NoSuchElementException();
		}
		res = buffer[ptrt.intValue()];
		ptrt = ptrt.declement();
		return res;
	}
	
	
	public int peek() {
//		if(ptrt.equals(ptrb)) {
//			throw new NoSuchElementException();
//		}
		return buffer[ptrt.intValue()];
	}
	
	
	public void put(int c) {
		buffer[ptrt.intValue()] = c;
	}
	
	
	public int get(int c) {
		if(c >= ptrt.intModulo()) {
			throw new IndexOutOfBoundsException("" + c);
		} else if(c > ptrt.subtract(ptrb).intValue()) {
			throw new NoSuchElementException();
		}
		return buffer[ptrt.subtract(c).intValue()];
	}
	
	
	public boolean isEmpty() {
		return ptrt.equals(ptrb);
	}
	
	
	public void clear() {
		ptrt = ptrb = IntResidue.valueOf(0, buffer.length);
		for(int i = 0; i < buffer.length; i++) {
			buffer[i] = 0;
		}
	}
	
}
