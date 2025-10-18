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
package net.morilib.math.order;

import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/11
 */
abstract class OrderElement implements Comparable<OrderElement> {

	//
	static final int FACT_LV = 91;
	static final int EXP_LV  = 85;
	static final int POLY_LV = 74;
	static final int LOG_LV  = 72;

	/**
	 * 
	 * @return
	 */
	public abstract int level();

	/**
	 * 
	 * @param o
	 * @return
	 */
	public abstract OrderElement multiply(OrderElement o);

	/**
	 * 
	 * @return
	 */
	public abstract Set<OrderElement> log();

	/**
	 * 
	 * @param b
	 * @return
	 */
	public abstract int compareLevel(OrderElement b);

	/**
	 * 
	 * @return
	 */
	public abstract boolean isConstant();

}
