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
 * This enumeration lists the possible gender types for nouns and proper names.
 * 
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public enum Gender {

	/**
	 * Undefined gender.
	 */
	UNDEF,

	/**
	 * Neutral gender.
	 */
	NEUTRAL,

	/**
	 * Human gender.
	 */
	HUMAN,

	/**
	 * Masculine gender.
	 */
	MASCULINE,

	/**
	 * Feminine gender.
	 */
	FEMININE;

	/**
	 * Returns the Prolog atom representation of the type of gender.
	 */
	public String toString() {
		switch (this) {
		case UNDEF: return "undef";
		case NEUTRAL: return "neutr";
		case HUMAN: return "human";
		case MASCULINE: return "masc";
		case FEMININE: return "fem";
		}
		return null;
	}


	/**
	 * Returns the gender that corresponds to the given Prolog atom.
	 */
	public static Gender create(String str) {
		if ("neutr".equals(str)) return NEUTRAL;
		if ("human".equals(str)) return HUMAN;
		if ("masc".equals(str)) return MASCULINE;
		if ("fem".equals(str)) return FEMININE;
		return UNDEF;
	}

}