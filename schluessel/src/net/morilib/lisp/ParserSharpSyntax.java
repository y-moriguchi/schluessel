/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

public abstract class ParserSharpSyntax {
	
	
	public static interface Engine {
		
		
		public boolean isMatch();
		
		
		public Datum getDatum();
		
		
		public boolean isFollowS();
		
		
		public boolean isUseMatch();
		
		
		public String getMatchString();
		
		
		public boolean isDead();
		
		
		public boolean isDeadNext(int c);
		
		
		public void go(int c);
		
	}
	
	//
	private static ParserSharpSyntax instance;
	
	
	protected ParserSharpSyntax() {
		// do nothing
	}
	
	
	public static final ParserSharpSyntax getInstance() {
		synchronized(ParserSharpSyntax.class) {
			if(instance == null) {
				instance = new ParserSharpSyntaxImpl();
			}
		}
		return instance;
	}
	
	
	public abstract Engine getEngine();
	
	
	public abstract void addRule(
			String pattern, Datum d, boolean follow, boolean use);
	
}
