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

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class ConsIterator implements Iterator<Datum> {

	//
	private Datum ptr;

	/**
	 * 
	 * @param d
	 */
	public ConsIterator(Datum d) {
		ptr = d;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return (ptr instanceof Cons);
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public Datum next() {
		if(ptr instanceof Cons) {
			Datum res = ((Cons)ptr).getCar();
			ptr = ((Cons)ptr).getCdr();

			return res;
		} else {
			throw new NoSuchElementException();
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public Datum getCar() {
		if(ptr instanceof Cons) {
			return ((Cons)ptr).getCar();
		} else {
			throw new NoSuchElementException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public Datum getTerminal() {
		return (ptr instanceof Cons) ? Undef.UNDEF : ptr;
	}

	/**
	 * 
	 * @return
	 */
	public Datum rest() {
		Datum r = ptr;

		ptr = Nil.NIL;
		return r;
	}

}
