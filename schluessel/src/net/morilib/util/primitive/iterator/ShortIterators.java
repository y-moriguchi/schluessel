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
 *
 * @author MORIGUCHI, Yuichiro 2010/11/03
 */
public final class ShortIterators {
	
	//
	private ShortIterators() { }
	
	//
	public static final ShortVectorIterator
	NULL_ITERATOR = new ShortVectorIterator() {

		public boolean hasNext() {
			return false;
		}

		public short next() {
			throw new NoSuchElementException();
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

		public void addShort(short v) {
			throw new UnsupportedOperationException();
		}

		public boolean hasPrevious() {
			return false;
		}

		public int nextIndex() {
			return 0;
		}

		public short previous() {
			throw new NoSuchElementException();
		}

		public int previousIndex() {
			return -1;
		}

		public void setShort(short v) {
			throw new UnsupportedOperationException();
		}

		public void add(int v) {
			throw new UnsupportedOperationException();
		}

		public void set(int v) {
			throw new UnsupportedOperationException();
		}
		
	};
	
	
	public static ShortIterator newIterator(final short[] a) {
		return new ShortIterator() {
			
			//
			private int p = 0;
			
			public boolean hasNext() {
				return p < a.length;
			}

			public short next() {
				return a[p++];
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}
	
	// %IS_Char%
//	public static CharacterIterator newIterator(final String s) {
//		return new CharacterIterator() {
//			
//			//
//			private int p = 0;
//			
//			@Override
//			public boolean hasNext() {
//				return p < s.length();
//			}
//
//			@Override
//			public char next() {
//				return s.charAt(p++);
//			}
//
//			@Override
//			public void remove() {
//				throw new UnsupportedOperationException();
//			}
//			
//		};
//	}
	// %IS_Char_END%
	
}
