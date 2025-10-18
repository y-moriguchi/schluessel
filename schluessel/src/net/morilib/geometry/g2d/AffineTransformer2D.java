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
import net.morilib.lang.number.NumericalField;
import net.morilib.lang.number.NumericalFieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/29
 */
public class AffineTransformer2D
<P extends Vector2D<P, F>, F extends FieldElement<F>>
implements VectorTransformer2D<P, F> {

	//
	private F scaleX, scaleY;
	private F shearX, shearY;
	private F transformX, transformY;

	/**
	 * 
	 * @param scaleX
	 * @param scaleY
	 * @param shearX
	 * @param shearY
	 * @param transformX
	 * @param transformY
	 */
	public AffineTransformer2D(
			F scaleX, F scaleY, F shearX, F shearY,
			F transformX, F transformY) {
		this.scaleX = scaleX;
		this.shearX = shearX;
		this.transformX = transformX;
		this.scaleY = scaleY;
		this.shearY = shearY;
		this.transformY = transformY;
	}

	/**
	 * 
	 * @param <P>
	 * @param <F>
	 * @param ff
	 * @param theta
	 * @return
	 */
	public static
	<P extends Vector2D<P, F>, F extends NumericalFieldElement<F>>
	AffineTransformer2D<P, F> getRotator(
			NumericalField<F> ff, double theta) {
		double sin = Math.sin(theta);
		double cos = Math.cos(theta);

		return new AffineTransformer2D<P, F>(
				ff.valueOf(cos),  ff.valueOf(cos),
				ff.valueOf(-sin), ff.valueOf(sin),
				ff.getZero(),     ff.getZero());
	}

	/**
	 * 
	 * @param <P>
	 * @param <F>
	 * @param ff
	 * @param anchor
	 * @param theta
	 * @return
	 */
	public static
	<P extends Vector2D<P, F>, F extends NumericalFieldElement<F>>
	AffineTransformer2D<P, F> getRotator(
			NumericalField<F> ff, P anchor, double theta) {
		double x = anchor.getX().doubleValue();
		double y = anchor.getY().doubleValue();
		double sin = Math.sin(theta);
		double cos = Math.cos(theta);

		return new AffineTransformer2D<P, F>(
				ff.valueOf(cos),  ff.valueOf(cos),
				ff.valueOf(-sin), ff.valueOf(sin),
				ff.valueOf(x - x * cos + y * sin),
				ff.valueOf(y - x * sin - y * cos));
	}

	/**
	 * @return the scaleX
	 */
	public F getScaleX() {
		return scaleX;
	}

	/**
	 * @return the scaleY
	 */
	public F getScaleY() {
		return scaleY;
	}

	/**
	 * @return the shearX
	 */
	public F getShearX() {
		return shearX;
	}

	/**
	 * @return the shearY
	 */
	public F getShearY() {
		return shearY;
	}

	/**
	 * @return the transformX
	 */
	public F getTransformX() {
		return transformX;
	}

	/**
	 * @return the transformY
	 */
	public F getTransformY() {
		return transformY;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.geometry.g2d.VectorTransformer2D#transform(net.morilib.geometry.g2d.Vector2D)
	 */
	public P transform(P p) {
		F x, y;

		x = scaleX.multiply(p.getX());
		x = x.add(shearX).multiply(p.getY());
		x = x.add(transformX);
		y = scaleY.multiply(p.getY());
		y = y.add(shearY).multiply(p.getX());
		y = y.add(transformY);
		return p.getFactory().create(x, y);
	}

}
