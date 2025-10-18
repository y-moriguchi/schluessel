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
package net.morilib.lang.transform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/23
 */
public final class DoubleMath {
	
	//
	private DoubleMath() { }
	
	/**
	 * 
	 */
	public static final DoubleTransform SIN = new DoubleTransform() {

		public double f(double a) {
			return Math.sin(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform COS = new DoubleTransform() {

		public double f(double a) {
			return Math.cos(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform TAN = new DoubleTransform() {

		public double f(double a) {
			return Math.tan(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform EXP = new DoubleTransform() {

		public double f(double a) {
			return Math.exp(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform LOG = new DoubleTransform() {

		public double f(double a) {
			return Math.log(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform ASIN = new DoubleTransform() {

		public double f(double a) {
			return Math.asin(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform ACOS = new DoubleTransform() {

		public double f(double a) {
			return Math.acos(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform ATAN = new DoubleTransform() {

		public double f(double a) {
			return Math.atan(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform SINH = new DoubleTransform() {

		public double f(double a) {
			return Math.sinh(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform COSH = new DoubleTransform() {

		public double f(double a) {
			return Math.cosh(a);
		}
		
	};
	
	/**
	 * 
	 */
	public static final DoubleTransform TANH = new DoubleTransform() {

		public double f(double a) {
			return Math.tanh(a);
		}
		
	};
	
}
