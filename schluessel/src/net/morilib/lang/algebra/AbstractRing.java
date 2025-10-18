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
package net.morilib.lang.algebra;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/16
 */
public abstract class AbstractRing<C extends RingElement<C>>
implements Ring<C> {
	
	
	public C add(C x, C y) {
		return (x != null && y != null) ? x.add(y) : null;
	}
	
	
	public C subtract(C x, C y) {
		return (x != null && y != null) ? x.subtract(y) : null;
	}
	
	
	public C multiply(C x, C y) {
		return (x != null && y != null) ? x.multiply(y) : null;
	}
	
	
	public C negate(C x) {
		return (x != null) ? x.negate() : null;
	}

}
