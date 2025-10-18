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
package net.morilib.lisp.java;

import java.lang.ref.Reference;

import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/19
 */
public final class LispWeakReference extends LispReference {

	//
	private Reference<? extends Datum> ref;

	/**
	 * 
	 * @param ref
	 */
	public LispWeakReference(Reference<? extends Datum> ref) {
		this.ref = ref;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.java.LispReference#get()
	 */
	@Override
	public Datum get() {
		return ref.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.java.LispReference#isCleared()
	 */
	@Override
	public boolean isCleared() {
		return ref.get() == null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.java.LispReference#clear()
	 */
	@Override
	public void clear() {
		ref.clear();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.java.LispReference#enqueue()
	 */
	@Override
	public boolean enqueue() {
		return ref.enqueue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.java.LispReference#isEnqueued()
	 */
	@Override
	public boolean isEnqueued() {
		return ref.isEnqueued();
	}

}
