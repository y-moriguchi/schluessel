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
package net.morilib.util;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/24
 */
public final class Characters {

	//
	private Characters() {}

	/**
	 * 
	 * @param ty
	 * @return
	 */
	public static String categoryToString(int ty) {
		switch(ty) {
		case Character.COMBINING_SPACING_MARK:    return "Mc";
		case Character.CONNECTOR_PUNCTUATION:     return "Pc";
		case Character.CONTROL:                   return "Cc";
		case Character.CURRENCY_SYMBOL:           return "Sc";
		case Character.DASH_PUNCTUATION:          return "Pd";
		case Character.DECIMAL_DIGIT_NUMBER:      return "Nd";
		case Character.ENCLOSING_MARK:            return "Me";
		case Character.END_PUNCTUATION:           return "Pe";
		case Character.FINAL_QUOTE_PUNCTUATION:   return "Pf";
		case Character.FORMAT:                    return "Cf";
		case Character.INITIAL_QUOTE_PUNCTUATION: return "Pi";
		case Character.LETTER_NUMBER:             return "Nl";
		case Character.LINE_SEPARATOR:            return "Zl";
		case Character.LOWERCASE_LETTER:          return "Ll";
		case Character.MATH_SYMBOL:               return "Sm";
		case Character.MODIFIER_LETTER:           return "Lm";
		case Character.MODIFIER_SYMBOL:           return "Sk";
		case Character.NON_SPACING_MARK:          return "Mn";
		case Character.OTHER_LETTER:              return "Lo";
		case Character.OTHER_NUMBER:              return "No";
		case Character.OTHER_PUNCTUATION:         return "Po";
		case Character.OTHER_SYMBOL:              return "So";
		case Character.PARAGRAPH_SEPARATOR:       return "Zp";
		case Character.PRIVATE_USE:               return "Co";
		case Character.SPACE_SEPARATOR:           return "Zs";
		case Character.START_PUNCTUATION:         return "Ps";
		case Character.SURROGATE:                 return "Cs";
		case Character.TITLECASE_LETTER:          return "Lt";
		case Character.UNASSIGNED:                return "Cn";
		case Character.UPPERCASE_LETTER:          return "Lu";
		default:  return null;
		}
	}

}
