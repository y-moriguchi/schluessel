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
package net.morilib.math.special;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import net.morilib.lang.transform.DoubleSequence;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/21
 */
public class ZetaFunctions {
	
	//
	private static final String ZETA_INTEGER = "zetainteger.txt";
	
	//
	static double calculateZ(int n) {
		double r = 0;
		double rold = Double.MAX_VALUE;
		int p = 2;
		
		while(Math.abs(r - rold) > Double.MIN_VALUE) {
			rold = r;
			r    = r + 1 / Math.pow(p, n);
			p++;
		}
		return r;
	}
	
	//
	static class _ZInt0 implements DoubleSequence {
		
		//
		double[] cache = new double[1000];
		
		public double f(int n) {
			if(n <= 1) {
				return Double.POSITIVE_INFINITY;
			} else if(n >= 1000 || cache[n] == 0) {
				double r = calculateZ(n);
				
				if(n < 1000) {
					cache[n] = r + 1;
				}
				return r + 1;
			} else {
				return cache[n];
			}
		}
		
	}
	
	//
	static class _ZIntMinus1 implements DoubleSequence {
		
		//
		double[] cache = new double[1024];
		
		//
		_ZIntMinus1() {
			String s = ZetaFunctions.class.getPackage().getName();
			BufferedReader rd = null;
			
			try {
				String l;
				s = "/" + s.replace('.', '/') + "/" + ZETA_INTEGER;
				rd = new BufferedReader(new InputStreamReader(
						ZetaFunctions.class.getResourceAsStream(s)));
				
				int i = 2;
				while((l = rd.readLine()) != null) {
					cache[i] = Double.parseDouble(l);
					i++;
				}
			} catch(IOException e) {
				throw new RuntimeException(e);
			} finally {
				if(rd != null) {
					try {
						rd.close();
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
		
		public double f(int n) {
			if(n <= 1) {
				return Double.POSITIVE_INFINITY;
			} else if(n >= cache.length || cache[n] == 0) {
				double r = calculateZ(n);
				
				if(n < cache.length) {
					cache[n] = r;
				}
				return r;
			} else {
				return cache[n];
			}
		}
		
	}
	
	//
	static class _ZInt implements DoubleSequence {
		
		public double f(int n) {
			if(n <= 1) {
				return Double.POSITIVE_INFINITY;
			} else {
				return RIEMANN_ZETA_MINUS1_SEQUENCE.f(n) + 1;
			}
		}
		
	}
	
	/**
	 * 
	 */
	public static final DoubleSequence RIEMANN_ZETA_SEQUENCE =
		new _ZInt();
	
	/**
	 * 
	 */
	public static final DoubleSequence RIEMANN_ZETA_MINUS1_SEQUENCE =
		new _ZIntMinus1();
	
}
