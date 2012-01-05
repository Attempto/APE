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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * This class provides an interface to the Attempto Parsing Engine (APE) socket server.
 * The socket server implementation is provided by <code>ape.exe</code>. To start
 * a server, execute for example:
 * 
 * <pre>
 * ./ape.exe -server -port 2766
 * </pre>
 * 
 * @author Kaarel Kaljurand
 */
public class APESocket extends ACEParser {

	private String host;
	private int port;

	/**
	 * Constructs a new parser object based on the hostname and the port number
	 * of the APE socket server.
	 * 
	 * @param host The hostname of the socket server.
	 * @param port The port number of the socket server.
	 */
	public APESocket(String host, int port) {
		this.host = host;
		this.port = port;
	}

	/**
	 * Constructs a new parser object based on the APE socket server running on "localhost".
	 * 
	 * @param port The port number of the socket server.
	 */
	public APESocket(int port) {
		this("localhost", port);
	}

	public ACEParserResult getMultiOutput(String text, Lexicon lexicon, OutputType... outputTypes) {
		String ulexStr = "";
		if (lexicon != null) {
			ulexStr = ",ulextext=" + PrologUtils.escape(lexicon.toString());
		}
		String paramList =  "text=" + PrologUtils.escape(text) + ulexStr + getOptions();
		for (OutputType t : outputTypes) {
			paramList = paramList + "," + t.toMultiFlag() + "=on";
		}
		return new ACEParserResult(getParserResponseAsString(paramList));
	}

	public String getSoloOutput(String text, Lexicon lexicon, OutputType outputType) throws ACEParserException {
		String ulexStr = "";
		if (lexicon != null) {
			ulexStr = ",ulextext=" + PrologUtils.escape(lexicon.toString());
		}
		String paramList = "text=" + PrologUtils.escape(text) + ulexStr + ",solo=" + outputType.toSoloFlag() + getOptions();
		return checkForErrors(getParserResponseAsString(paramList));
	}


	private String getParserResponseAsString(String paramList) {
		Socket client;
		try {
			client = new Socket(host, port);
		} catch (UnknownHostException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		} catch (IOException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		}


		BufferedReader fromServer;

		try {
			fromServer = new BufferedReader(new InputStreamReader(client.getInputStream()));
		} catch (IOException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		}


		PrintWriter toServer;
		try {
			toServer = new PrintWriter(client.getOutputStream(), true);
		} catch (IOException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		}

		toServer.println("get([" + paramList + "]).");

		String allLines = "";
		String responseLine;
		try {
			while ((responseLine = fromServer.readLine()) != null) {
				if (responseLine.equals("APESERVERSTREAMEND")) {
					break;
				}
				// TODO: not sure if we should replace "\n" with a platform independent method call
				allLines = allLines + responseLine + "\n";
			}
		} catch (IOException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		}
		try {
			toServer.close();
			fromServer.close();
			client.close();
		} catch (IOException e) {
			throw new RuntimeException("Accessing APE socket failed: " + e.getMessage());
		}
		return allLines;
	}
}