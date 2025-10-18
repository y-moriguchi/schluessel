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
package net.morilib.util.set;

import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;

import net.morilib.util.Objects;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/05
 */
public abstract class Flat3CachedSet<E> extends CachedSet<E> {
	
	//
	private E ch1 = null, ch2 = null, ch3 = null;
	private int sizeModCount = 0;    // modCount(16bit) + size(16bit)
	
	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#cacheIterator()
	 */
	@Override
	protected Iterator<E> cacheIterator() {
		return new Iterator<E>() {
			
			// modcount(16bit) + size(16bit)
			private int ptrExModCount = sizeModCount & 0xffff0000;
			private boolean removed = true;
			
			public boolean hasNext() {
				return (ptrExModCount & 0xffff) < cacheSize();
			}

			public E next() {
				if(!hasNext()) {
					throw new NoSuchElementException();
				}
				
				removed = false;
				switch(ptrExModCount++ & 0xffff) {
				case 0:  return ch1;
				case 1:  return ch2;
				case 2:  return ch3;
				default: throw new IllegalStateException();
				}
			}

			public void remove() {
				int p = ptrExModCount;
				int s = sizeModCount;
				
				if(removed) {
					throw new IllegalStateException();
				} else if((p & 0xffff0000) != (s & 0xffff0000)) {
					throw new ConcurrentModificationException();
				}
				removeCache((p & 0x0000ffff) - 1);
				ptrExModCount &= 0x0000ffff;
				ptrExModCount |= sizeModCount & 0xffff0000;
				ptrExModCount--;
				removed = true;
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#cacheSize()
	 */
	@Override
	protected int cacheSize() {
		return sizeModCount & 0xffff;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#isCacheFull()
	 */
	@Override
	protected boolean isCacheFull() {
		return (sizeModCount & 0xffff) == 3;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#hitCache(java.lang.Object)
	 */
	@Override
	protected int hitCache(Object e) {
		int z = sizeModCount & 0xffff;
		
		switch(z) {
		case 0:   return -1;
		case 1:   return Objects.equals(e, ch1) ? 0 : -1;
		case 2:
			return Objects.equals(e, ch1) ?
					0 : Objects.equals(e, ch2) ? 1 : -1;
		default:
			return Objects.equals(e, ch1) ?
					0 : Objects.equals(e, ch2) ?
							1 : Objects.equals(e, ch3) ? 2 : -1;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#getCache(int)
	 */
	@Override
	protected E getCache(int i) {
		switch(i) {
		case 0:  return ch1;
		case 1:  return ch2;
		case 2:  return ch3;
		default: throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#addCache(java.lang.Object)
	 */
	@Override
	protected void addCache(E e) {
		int z = sizeModCount & 0xffff;
		
		switch(z) {
		case 0:   ch1 = e;  break;
		case 1:   ch2 = e;  break;
		case 2:   ch3 = e;  break;
		default:  throw new IllegalStateException();
		}
		sizeModCount += 0x00010001;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#removeCache(int)
	 */
	@Override
	protected void removeCache(int i) {
		if(i >= (sizeModCount & 0xffff)) {
			throw new IllegalArgumentException();
		}
		
		switch(i) {
		case 0:  ch1 = ch2;   // continue
		case 1:  ch2 = ch3;   // continue
		case 2:  ch3 = null;  break;
		default: throw new IllegalArgumentException();
		}
		sizeModCount += 0x0000ffff;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#setCache(int, java.lang.Object)
	 */
	@Override
	protected void setCache(int i, E e) {
		if(i >= (sizeModCount & 0xffff)) {
			throw new IllegalArgumentException();
		}
		
		switch(i) {
		case 0:  ch1 = e;  break;
		case 1:  ch2 = e;  break;
		case 2:  ch3 = e;  break;
		default: throw new IllegalArgumentException();
		}
		sizeModCount += 0x00010000;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#clearCache()
	 */
	@Override
	protected void clearCache() {
		ch1 = ch2 = ch3 = null;
		sizeModCount &= 0xffff0000;
		sizeModCount += 0x00010000;
	}

}
