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
package net.morilib.util.primitive;

import java.util.Collection;

import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface DoubleCollection extends Collection<Double> {
	
	public boolean addDouble(double v);
	
	public boolean add(int v);
	
	public boolean addAllDouble(DoubleCollection a);
	
	public boolean addAllDouble(DoubleCollection... as);
	
	public boolean addAllDouble(
			Collection<? extends DoubleCollection> as);
	
	public void clear();
	
	public boolean containsDouble(double v);
	
	public boolean contains(int v);
	
	public boolean containsAllDouble(DoubleCollection a);
	
	public boolean isEmpty();
	
	public DoubleIterator doubleIterator();
	
	public boolean removeDouble(double v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllDouble(DoubleCollection a);
	
	public boolean retainAllDouble(DoubleCollection a);
	
	public int size();
	
	public double[] toDoubleArray();
	
	public double[] toDoubleArray(double[] a);
	
	public boolean isInfinite();
	
	public DoubleSet toSet();
	
}
