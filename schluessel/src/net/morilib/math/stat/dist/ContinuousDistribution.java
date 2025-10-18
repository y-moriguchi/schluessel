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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public interface ContinuousDistribution extends Distribution {

	/**
	 * 
	 * @param n
	 * @return
	 */
	public boolean isInSupport(double n);

	/**
	 * 
	 * @param p
	 * @return
	 */
	public double invCdf(double p);

	/**
	 * 
	 * @return
	 */
	public double supportSupremum();

	/**
	 * 
	 * @return
	 */
	public double supportInfimum();

}
