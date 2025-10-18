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
package net.morilib.util.primitive.iterator;

import java.util.NoSuchElementException;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/11/03
 */
public class LongIterators {
	
	//
	public static final LongVectorIterator
	NULL_ITERATOR = new LongVectorIterator() {

		public boolean hasNext() {
			return false;
		}

		public long next() {
			throw new NoSuchElementException();
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

		public void addLong(long v) {
			throw new UnsupportedOperationException();
		}

		public boolean hasPrevious() {
			return false;
		}

		public int nextIndex() {
			return 0;
		}

		public long previous() {
			throw new NoSuchElementException();
		}

		public int previousIndex() {
			return -1;
		}

		public void setLong(long v) {
			throw new UnsupportedOperationException();
		}

		public void add(int v) {
			throw new UnsupportedOperationException();
		}

		public void set(int v) {
			throw new UnsupportedOperationException();
		}
		
	};
	
}
