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
package net.morilib.lisp.math.matrix;

import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.math.algebra.ILispInnerProduct;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.math.algebra.ILispVectorSpace;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public interface ILispNumberVector
extends ILispVector, Iterable<LispNumber>,
ILispVectorSpace<ILispNumberVector>,
ILispInnerProduct<ILispNumberVector>,
ILispNumberEqual<ILispNumberVector> {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#get(int)
	 */
	public LispNumber get(int index);

	/**
	 * 
	 * @param index
	 * @param x
	 * @throws LispMatrixException
	 */
	public void set(int index,
			LispNumber x) throws LispMatrixException;

	/**
	 * 
	 * @param x
	 * @return
	 */
	public ILispNumberVector mul(LispNumber x);

	/**
	 * 
	 * @param v
	 * @return
	 */
	public ILispNumberVector add(ILispNumberVector v);

	/**
	 * 
	 * @param v
	 * @return
	 */
	public ILispNumberVector sub(ILispNumberVector v);

	/**
	 * 
	 * @return
	 */
	public ILispNumberVector uminus();

	/**
	 * 
	 * @param b
	 * @return
	 * @throws LispMatrixException
	 */
	public LispNumber innerProduct(
			ILispNumberVector b) throws LispMatrixException;

	/**
	 * 
	 * @return
	 */
	public LispNumber normSquared();

	/**
	 * 
	 * @param y
	 * @return
	 */
	public boolean isEqualTo(ILispNumberVector y);

	/**
	 * 
	 * @return
	 */
	public boolean isQuaternionVector();

	/**
	 * 
	 * @return
	 */
	public boolean isComplexVector();

	/**
	 * 
	 * @return
	 */
	public boolean isRealVector();

}
