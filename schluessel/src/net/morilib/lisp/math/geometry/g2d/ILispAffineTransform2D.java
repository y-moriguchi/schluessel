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
package net.morilib.lisp.math.geometry.g2d;

import java.awt.geom.AffineTransform;

import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.matrix.ILispMatrix;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/17
 */
public interface ILispAffineTransform2D extends ILispMatrix {

	/**
	 * @return the scaleX
	 */
	public LispReal getScaleX();

	/**
	 * @return the scaleY
	 */
	public LispReal getScaleY();

	/**
	 * @return the shearX
	 */
	public LispReal getShearX();

	/**
	 * @return the shearY
	 */
	public LispReal getShearY();

	/**
	 * @return the transformX
	 */
	public LispReal getTranslateX();

	/**
	 * @return the transformY
	 */
	public LispReal getTranslateY();

	/**
	 * @return the scaleX
	 */
	public double getScaleXDouble();

	/**
	 * @return the scaleY
	 */
	public double getScaleYDouble();

	/**
	 * @return the shearX
	 */
	public double getShearXDouble();

	/**
	 * @return the shearY
	 */
	public double getShearYDouble();

	/**
	 * @return the transformX
	 */
	public double getTranslateXDouble();

	/**
	 * @return the transformY
	 */
	public double getTranslateYDouble();

	/**
	 * 
	 * @param rad
	 * @return
	 */
	public ILispAffineTransform2D rotateByRadian(double rad);

	/**
	 * 
	 * @param deg
	 * @return
	 */
	public ILispAffineTransform2D rotateByIntDegree(int deg);

	/**
	 * 
	 * @param sx
	 * @param sy
	 * @return
	 */
	public ILispAffineTransform2D scale(double sx, double sy);

	/**
	 * 
	 * @param sx
	 * @param sy
	 * @return
	 */
	public ILispAffineTransform2D shear(double sx, double sy);

	/**
	 * 
	 * @param tx
	 * @param ty
	 * @return
	 */
	public ILispAffineTransform2D translate(double tx, double ty);

	/**
	 * 
	 * @return
	 */
	public ILispAffineTransform2D reflectX();

	/**
	 * 
	 * @return
	 */
	public ILispAffineTransform2D reflectY();

	/**
	 * 
	 * @param k
	 * @return
	 */
	public ILispAffineTransform2D squeeze(double k);

	/**
	 * 
	 * @param t
	 * @return
	 */
	public ILispAffineTransform2D concatenate(
			ILispAffineTransform2D t);

	/**
	 * 
	 * @return
	 */
	public AffineTransform toAWTTransform();

}
