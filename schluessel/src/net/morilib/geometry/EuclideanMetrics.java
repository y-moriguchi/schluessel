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
package net.morilib.geometry;

import net.morilib.geometry.g2d.Vector2D;
import net.morilib.lang.algebra.FieldElement;
import net.morilib.lang.algebra.HasSquareRoot;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/29
 */
public class EuclideanMetrics {

	/**
	 * 
	 * @param <F>
	 * @param p1
	 * @param p2
	 * @return
	 */
	public static final
	<P extends Vector2D<P, F>, F extends FieldElement<F>>
	F norm2(Vector2D<P, F> p1, Vector2D<P, F> p2) {
		F res;

		res = p1.getX().subtract(p2.getX()).power(2);
		res = res.add(p1.getY().subtract(p2.getY()).power(2));
		return res;
	}

	/**
	 * 
	 * @param <F>
	 * @param p1
	 * @param p2
	 * @return
	 */
	public static final
	<P extends Vector2D<P, F>,
	 F extends FieldElement<F> & HasSquareRoot<F>>
	F norm(Vector2D<P, F> p1, Vector2D<P, F> p2) {
		return norm2(p1, p2).sqrt();
	}

}
