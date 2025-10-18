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
package net.morilib.math.stat.dist;

import net.morilib.lang.transform.DoubleTransform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public interface Distribution extends DoubleTransform {

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double cdf(double x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double cdf(double x1, double x2);

	/**
	 * 
	 * @return
	 */
	public double expectedValue();

	/**
	 * 
	 * @return
	 */
	public double variance();

	/**
	 * 
	 * @return
	 */
	public double mode();

	/**
	 * 
	 * @return
	 */
	public double median();

	/**
	 * 
	 * @return
	 */
	public double skewness();

	/**
	 * 
	 * @return
	 */
	public double kurtosis();

}
