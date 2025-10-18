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
package net.morilib.math;

import net.morilib.util.SimpleMap;

/**
 * An interface represents an element of a group.
 * <p>群の要素を表現するインターフェースです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public interface GroupElement<E> extends SimpleMap<E, E> {
	
	/**
	 * gets the inverse of this element.
	 * <p>この要素の逆元を取得します。
	 * 
	 * @return inverse
	 */
	public E invert();
	
	/**
	 * returns true if this element is equal to the given element.
	 * <p>与えられた要素がこの要素と等しいときtrueを得ます。
	 * 
	 * @param e an element to be tested
	 */
	public boolean isEqualTo(E e);
	
}
