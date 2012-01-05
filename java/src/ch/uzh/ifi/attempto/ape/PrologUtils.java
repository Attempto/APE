// This file is part of the Attempto Parsing Engine (APE).
// Copyright 2008-2012, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later version.
//
// The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.ape;

/**
 * This class contains Prolog-related utility methods.
 * 
 * @author Tobias Kuhn
 */
public class PrologUtils {

	private PrologUtils() {}  // no instances allowed

	/**
	 * Escapes the string according to Prolog escaping rules and puts it into single quotes.
	 * 
	 * @param s The string to be escaped.
	 * @return The escaped string.
	 */
	public static String escape(String s) {
		if (s == null) {
			return "''";
		}
		return "'" + s.replace("\\", "\\\\").replace("'", "\\'") + "'";
	}

}