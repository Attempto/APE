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

import java.io.StringReader;

import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * This is an interface to the Attempto Parsing Engine (APE), that translates sentences in
 * Attempto Controlled English (ACE) into logic.
 * 
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public abstract class ACEParser {

	private boolean guessingEnabled = false;
	private boolean clexEnabled = true;
	private String uri;

	/**
	 * Creates a new ACEParser object.
	 */
	protected ACEParser() {}

	/**
	 * Returns one single output for the given ACE text using the given lexicon. See the enumeration
	 * OutputType for further information about the possible outputs. If parsing succeeds then a string
	 * containing the result of the parser is returned. In the case of failure, an ACEParserException
	 * is thrown containing the error messages.
	 * 
	 * @param text The ACE text to be parsed.
	 * @param lexicon The lexicon to be loaded.
	 * @param outputType The kind of output that should be returned.
	 * @return The result as a string.
	 * @throws ACEParserException Contains the error messages if an error occurred.
	 * @see #getMultiOutput(String, Lexicon, OutputType...)
	 */
	public abstract String getSoloOutput(String text, Lexicon lexicon, OutputType outputType) throws ACEParserException;

	/**
	 * Returns one single output for the given ACE text using no lexicon.
	 * 
	 * @param text The ACE text to be parsed.
	 * @param outputType The kind of output that should be returned.
	 * @return The result as a string.
	 * @throws ACEParserException Contains the error messages if an error occurred.
	 * @see #getSoloOutput(String, Lexicon, OutputType)
	 */
	public final String getSoloOutput(String text, OutputType outputType) throws ACEParserException {
		return getSoloOutput(text, null, outputType);
	}

	/**
	 * Returns multiple outputs for the given ACE text using the given lexicon. See the enumeration
	 * OutputType for further information about the possible outputs. The result is returned as an
	 * ACEParserResult object.
	 * 
	 * @param text The ACE text to be parsed.
	 * @param lexicon The lexicon to be loaded.
	 * @param outputTypes The kind of outputs that should be returned.
	 * @return A ParserResult object containing the outputs.
	 * @see #getSoloOutput(String, Lexicon, OutputType)
	 */
	public abstract ACEParserResult getMultiOutput(String text, Lexicon lexicon, OutputType... outputTypes);

	/**
	 * Returns multiple outputs for the given ACE text using no lexicon.
	 * 
	 * @param text The ACE text to be parsed.
	 * @param outputTypes The kind of outputs that should be returned.
	 * @return A ParserResult object containing the outputs.
	 * @see #getMultiOutput(String, Lexicon, OutputType...)
	 */
	public final ACEParserResult getMultiOutput(String text, OutputType... outputTypes) {
		return getMultiOutput(text, null, outputTypes);
	}

	/**
	 * Determines whether unknown words should be guessed. If false, unknown words lead to an error
	 * message. Note that unknown word guessing is not always perfect. The default value is false.
	 * 
	 * @param guessingEnabled true if unknown words should be guessed. false otherwise.
	 */
	public void setGuessingEnabled(boolean guessingEnabled) {
		this.guessingEnabled = guessingEnabled;
	}

	/**
	 * Returns whether unknown words should be guessed.
	 * 
	 * @return true if guessing is enabled.
	 */
	public boolean isGuessingEnabled() {
		return guessingEnabled;
	}

	/**
	 * Sets the namespace URI to be used for outputs like OWL.
	 * 
	 * @param uri The namespace URI.
	 */
	public void setURI(String uri) {
		// TODO: BUG: should we use a URI instead of String?
		this.uri = uri;
	}

	/**
	 * Returns the namespace URI to be used for outputs like OWL.
	 * 
	 * @return The namespace URI.
	 */
	public String getURI() {
		return uri;
	}

	/**
	 * Determines whether the built-in lexicon should be used. The default value is true.
	 * 
	 * @param clexEnabled true if the built-in lexicon should be used. false otherwise.
	 */
	public void setClexEnabled(boolean clexEnabled) {
		this.clexEnabled  = clexEnabled;
	}

	/**
	 * Returns whether the built-in lexicon is used.
	 * 
	 * @return true if the built-in lexicon is used.
	 */
	public boolean isClexEnabled() {
		return clexEnabled;
	}

	String getOptions() {
		String guessString = "";
		if (isGuessingEnabled()) guessString = ",guess=on";
		String clexString = ",noclex=on";
		if (isClexEnabled()) clexString = ",noclex=off";
		String uriString = "";
		if (getURI() != null) {
			uriString = ",uri=" + PrologUtils.escape(getURI());
		}
		return guessString + clexString + uriString;
	}

	static String checkForErrors(String s) throws ACEParserException {
		Element xmlElement = null;
		try {
			SAXBuilder sb = new SAXBuilder();
			sb.setValidation(false);
			xmlElement = sb.build(new StringReader(s)).getRootElement();
		} catch (Exception ex) {
			// String s does not represent an XML document 
			return s;
		}

		if (xmlElement != null && xmlElement.getName().equals("messages")) {
			throw new ACEParserException(xmlElement);
		}
		return s;
	}
}