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
package net.morilib.lisp.math.geometry.g3d;

import net.morilib.lisp.math.algebra.ILispNorm;
import net.morilib.lisp.math.matrix.ILispNumberVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public interface ILispVector3D extends ILispNumberVector, ILispNorm {

	/**
	 * 
	 * @return
	 */
	public ILispVector3D crossProduct(ILispVector3D x);

}
