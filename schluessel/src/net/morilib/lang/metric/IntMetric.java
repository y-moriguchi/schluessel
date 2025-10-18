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
package net.morilib.lang.metric;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/30
 */
public interface IntMetric<T extends IntMetric<T>> {

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public byte byteMetric(T a, T b);

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public short shortMetric(T a, T b);

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public int intMetric(T a, T b);

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public long longMetric(T a, T b);

}
