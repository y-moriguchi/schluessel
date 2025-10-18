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
package net.morilib.geometry.g2d;

import net.morilib.lang.algebra.FieldElement;
import net.morilib.lang.algebra.VectorElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public interface Vector2D
<P extends Vector2D<P, F>, F extends FieldElement<F>>
extends VectorElement<P, F> {

	/**
	 * @return the x
	 */
	public F getX();

	/**
	 * @return the y
	 */
	public F getY();

	/**
	 * 
	 * @param p
	 * @return
	 */
	public F innerProduct(P p);

	/**
	 * 
	 * @param p
	 * @return
	 */
	public F crossProduct(P p);

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public VectorFactory2D<P, F> getFactory();

}
