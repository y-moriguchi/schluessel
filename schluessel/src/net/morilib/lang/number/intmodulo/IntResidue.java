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
package net.morilib.lang.number.intmodulo;

import net.morilib.lang.Hashes;
import net.morilib.util.IntMath;

public class IntResidue implements ResidueRingElement<IntResidue> {
	
	//
	private int value;
	private int modulo;
	
	
	protected IntResidue(int value, int modulo) {
		if(modulo <= 0) {
			throw new InvalidModuloException();
		}
		this.value  = mod(value, modulo);
		this.modulo = modulo;
	}
	
	//
	private static int mod(int v, int md) {
		int w;
		
		w = v % md;
		return (w < 0) ? md + w : w;
	}
	
	
	public static IntResidue valueOf(int value, int modulo) {
		return new IntResidue(value, modulo);
	}
	
	
	public boolean isZero() {
		return value == 0;
	}
	

	public IntResidue negate() {
		return new IntResidue((modulo - value) % modulo, modulo);
	}
	

	public IntResidue subtract(int n) {
		return new IntResidue(mod(value - n, modulo), modulo);
	}
	

	public IntResidue subtract(IntResidue x) {
		if(modulo != x.modulo) {
			throw new InvalidModuloException();
		}
		return new IntResidue(mod(value - x.value, modulo), modulo);
	}
	
	
	public IntResidue add(int n) {
		return new IntResidue(mod(value + n, modulo), modulo);
	}
	

	public IntResidue add(IntResidue x) {
		if(modulo != x.modulo) {
			throw new InvalidModuloException();
		}
		return new IntResidue(mod(value + x.value, modulo), modulo);
	}

	
	public IntResidue multiply(int n) {
		return new IntResidue(mod(value * n, modulo), modulo);
	}

	
	public IntResidue multiply(IntResidue x) {
		if(modulo != x.modulo) {
			throw new InvalidModuloException();
		}
		return new IntResidue(mod(value * x.value, modulo), modulo);
	}

	
	public IntResidue power(int n) {
		return new IntResidue(
				mod(IntMath.pow(value, n), modulo), modulo);
	}

	
	public IntResidue declement() {
		return new IntResidue(mod(value - 1, modulo), modulo);
	}

	
	public IntResidue inclement() {
		return new IntResidue(mod(value + 1, modulo), modulo);
	}
	
	
	public boolean isCongruent(int n) {
		return value == mod(n, modulo);
	}
	
	
	public int intValue() {
		return value;
	}
	
	
	public int intModulo() {
		return modulo;
	}
	
	
	public boolean equals(Object o) {
		if(o instanceof IntResidue) {
			IntResidue r = (IntResidue)o;
			
			return value == r.value && modulo == r.modulo;
		}
		return false;
	}
	
	
	public int hashCode() {
		int r = Hashes.INIT;
		
		r = (r * Hashes.A) + value;
		r = (r * Hashes.A) + modulo;
		return r;
	}
	
	
	public String toString() {
		return value + "(mod " + modulo + ")";
	}

}
