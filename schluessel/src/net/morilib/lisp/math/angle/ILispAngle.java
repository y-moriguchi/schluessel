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
package net.morilib.lisp.math.angle;

import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.ILispExactOrInexactQuantity;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispNegatable;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.math.algebra.ILispScalarMultipliable;
import net.morilib.lisp.math.algebra.ILispSubtractable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public interface ILispAngle extends ILispAddable<ILispAngle>,
ILispSubtractable<ILispAngle>, ILispNegatable<ILispAngle>,
ILispScalarMultipliable<ILispAngle>, Comparable<ILispAngle>,
ILispExactOrInexactQuantity<ILispAngle>,
ILispNumberEqual<ILispAngle> {

	/**
	 * 
	 * @return
	 */
	public LispReal byRadian();

	/**
	 * 
	 * @return
	 */
	public LispReal byDegree();

	/**
	 * 
	 * @return
	 */
	public LispReal byGrade();

	/**
	 * 
	 * @return
	 */
	public LispReal sin();

	/**
	 * 
	 * @return
	 */
	public LispReal cos();

	/**
	 * 
	 * @return
	 */
	public LispReal tan();

	/**
	 * 
	 * @return
	 */
	public LispReal cot();

	/**
	 * 
	 * @return
	 */
	public LispReal sec();

	/**
	 * 
	 * @return
	 */
	public LispReal cosec();

	/**
	 * 
	 * @return
	 */
	public boolean isAcute();

	/**
	 * 
	 * @return
	 */
	public boolean isRight();

	/**
	 * 
	 * @return
	 */
	public boolean isObtuse();

	/**
	 * 
	 * @return
	 */
	public boolean isStraight();

	/**
	 * 
	 * @return
	 */
	public boolean isReflex();

	/**
	 * 
	 * @return
	 */
	public boolean isSinExact();

	/**
	 * 
	 * @return
	 */
	public boolean isCosExact();

	/**
	 * 
	 * @return
	 */
	public boolean isTanExact();

}
