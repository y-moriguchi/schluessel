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
package net.morilib.lisp.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;

public class ConsIterable implements Iterable<Datum> {
	
	//
	private class Iter implements Iterator<Datum> {
		
		private Datum ptr;
		
		private Iter(Datum stp) {
			ptr = stp;
		}
		
		public boolean hasNext() {
			if(ptr instanceof Cons) {
				return true;
			} else if(ptr == Nil.NIL) {
				return false;
			} else if(mesg == null) {
				return false;
			} else {
				throw mesg.getError("err.list", stp);
			}
		}

		public Datum next() {
			if(ptr instanceof Cons) {
				Cons c0 = (Cons)ptr;
				
				ptr = c0.getCdr();
				return c0.getCar();
			} else {
				throw new NoSuchElementException();
			}
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
	
	//
	private LispMessage mesg;
	private Datum       stp;
	
	
	public ConsIterable(Datum dt, LispMessage mesg) {
		this.stp = dt;
		this.mesg = mesg;
	}


	public Iterator<Datum> iterator() {
		return new Iter(stp);
	}
	
}
