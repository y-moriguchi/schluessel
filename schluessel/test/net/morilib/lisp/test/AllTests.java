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
package net.morilib.lisp.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for net.morilib.lisp.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ParserTest.class);
		suite.addTestSuite(R5RSTest.class);
		//suite.addTestSuite(SubrListTest.class);
		suite.addTestSuite(MiscTest.class);
		suite.addTestSuite(SyntaxRulesTest.class);
		//suite.addTestSuite(NumberTest.class);
		suite.addTestSuite(TailRecursionTest.class);
		suite.addTestSuite(ContinuationTest.class);
		//suite.addTestSuite(MathTest.class);
		//suite.addTestSuite(CharacterTest.class);
		//suite.addTestSuite(JavaReflectTest.class);
		//suite.addTestSuite(StringToNumberTest.class);
		suite.addTestSuite(SyntaxTest.class);
		//suite.addTestSuite(StringTest.class);
		//suite.addTestSuite(VectorTest.class);
		//suite.addTestSuite(PortTest.class);
		//$JUnit-END$
		return suite;
	}

}
