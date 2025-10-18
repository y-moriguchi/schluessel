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
package net.morilib.lisp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public abstract class LispIrrational extends LispExactReal {

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isInfinity()
	 */
	@Override
	public boolean isInfinity() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isNaN()
	 */
	@Override
	public boolean isNaN() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isOne()
	 */
	@Override
	public boolean isOne() {
		return false;
	}

}
