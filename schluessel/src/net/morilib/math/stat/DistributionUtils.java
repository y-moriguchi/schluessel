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
package net.morilib.math.stat;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/18
 */
public class DistributionUtils {

	/**
	 * 
	 * @param x
	 * @param epsilon
	 * @return
	 */
	public static double trimCdf(double x, double epsilon) {
		if(Math.abs(x) < epsilon) {
			return 0.0;
		} else if(Math.abs(x - 1.0) < epsilon) {
			return 1.0;
		} else {
			return x;
		}
	}

}
