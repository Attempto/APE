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

import jpl.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class provides an interface to the SWI Prolog executable of the Attempto Parsing Engine
 * (APE). Note that you need the file "ape.exe" (which can be compiled from the Attempto APE
 * distribution) and that SWI Prolog needs to be installed.
 * Furthermore, you have to make sure that the JPL libraries of SWI Prolog are in the java library
 * path. This can be achieved by a Java VM argument which looks like this (for Mac OS X):
 * <p/>
 * <blockquote><code>
 * -Djava.library.path=/opt/local/lib/swipl-5.6.45/lib/i386-darwin8.10.1
 * </code></blockquote>
 * <p/>
 * Under Linux, the following environment variable has to be set additionally:
 * <p/>
 * <blockquote><code>
 * LD_PRELOAD=/usr/lib/pl-5.6.45/lib/i386-linux/libjpl.so
 * </code></blockquote>
 * <p/>
 * The exact paths are most probably different on your machine. Just look for the directory that
 * contains the file or symbolic link <em>libjpl.jnilib</em> (under Mac OS X), <em>jpl.dll</em>
 * (under Windows), or <em>libjpl.so</em> (under Unix). If you get the error message
 * <p/>
 * <blockquote><code>
 * java.lang.UnsatisfiedLinkError: no jpl in java.library.path
 * </code></blockquote>
 * <p/>
 * then this is a strong indication that the SWI Prolog JPL libraries are not found.
 *
 * @author Tobias Kuhn
 */
public class APELocal extends ACEParser {

    private static APELocal apeLocal;
    private static Map<String, String> swiplRuntimeVariables;

    /**
     * Creates a new parser object. You can create at most one such object per JVM. If you
     * try to create more than one such object or if you called the init method before
     * then a runtime exception is thrown.
     *
     * @param apeExeFile The path (with filename) of the file "ape.exe".
     */
    private APELocal(String apeExeFile) {
        if (apeLocal != null) {
            throw new RuntimeException("Only one APELocal object can be created.");
        }

        JPL.setNativeLibraryDir(getSwiplLibraryDir());
        // A problem was reported that might be solved by using the --nosignals option.
        // Otherwise, it seems to have no effect in our case.
        // The first argument "swipl" has no effect and could be any other string.

        JPL.init(new String[]{"swipl", "-x", apeExeFile, "-g", "true", "--nosignals"});

        apeLocal = this;
    }

    /**
     * Creates a new parser object. You can create at most one such object per JVM. If you
     * try to create more than one such object or if you called the init method before
     * then a runtime exception is thrown.
     * <br>
     * This method is deprecated because the {@code prologCommand} argument is actually not needed.
     * Use {@link #APELocal(String)} instead.
     *
     * @param prologCommand The command to run the SWI Prolog interpreter.
     *                      On Windows this is usually "plcon", on Linux "pl", and on Mac "swipl".
     * @param apeExeFile    The path (with filename) of the file "ape.exe".
     */
    @Deprecated
    public APELocal(String prologCommand, String apeExeFile) {
        this(apeExeFile);
    }

    /**
     * Returns the singleton APELocal instance. Null is returned if the APELocal instance has not yet
     * been initialized by the constructor or the init method.
     *
     * @return The singleton APELocal instance.
     */
    public static synchronized APELocal getInstance() {
        return apeLocal;
    }

    /**
     * Initializes the APELocal singleton instance. This method can be called at most once.
     * If you call it twice or if you called the constructor before then a runtime exception
     * is thrown.
     *
     * @param apeExeFile The path (with filename) of the file "ape.exe".
     */
    public synchronized static void init(String apeExeFile) {
        if (apeLocal == null) {
            apeLocal = new APELocal(apeExeFile);
        }
    }

    /**
     * Initializes the APELocal singleton instance. This method can be called at most once.
     * If you call it twice or if you called the constructor before then a runtime exception
     * is thrown.
     * <br>
     * This method is deprecated because the {@code prologCommand} argument is actually not needed.
     * Use {@link #init(String)} instead.
     *
     * @param prologCommand The command to run the SWI Prolog interpreter.
     *                      On Windows this is usually "plcon", on Linux "pl", and on Mac "swipl".
     * @param apeExeFile    The path (with filename) of the file "ape.exe".
     */
    @Deprecated
    public static void init(String prologCommand, String apeExeFile) {
        new APELocal(prologCommand, apeExeFile);
    }

    /**
     * Checks whether the singleton instance has already been initialized.
     *
     * @return true if the singleton instance has been initialized.
     */
    public static synchronized boolean isInitialized() {
        return apeLocal != null;
    }

    public synchronized String getSoloOutput(String text, Lexicon lexicon, OutputType outputType) throws ACEParserException {
        clearMessages();
        String ulextext = "";
        if (lexicon != null) {
            ulextext = ",ulextext=" + PrologUtils.escape(lexicon.toString());
        }
        Term input = Util.textToTerm("[text=" + PrologUtils.escape(text) + ulextext + ",solo=" + outputType.toString().toLowerCase() + getOptions() + "]");
        Query q = new Query("get_ape_results", new Term[]{input, new Variable("Result")});
        Hashtable hashtable = q.oneSolution();
        Atom result = (Atom) hashtable.get("Result");
        String s = result.name();

        return checkForErrors(s);
    }

    public synchronized ACEParserResult getMultiOutput(String text, Lexicon lexicon, OutputType... outputTypes) {
        clearMessages();
        String outputs = "";
        for (OutputType t : outputTypes) {
            outputs += ",c" + t.toString().toLowerCase() + "=on";
        }
        String ulextext = "";
        if (lexicon != null) {
            ulextext = ",ulextext=" + PrologUtils.escape(lexicon.toString());
        }
        Term input = Util.textToTerm("[text=" + PrologUtils.escape(text) + ulextext + outputs + getOptions() + "]");
        Query q = new Query("get_ape_results", new Term[]{input, new Variable("Result")});
        Atom result = (Atom) q.oneSolution().get("Result");
        return new ACEParserResult(result.name());
    }

    /**
     * Loads the lexicon by adding all lexicon entries of the lexicon. Note that these lexicon entries
     * are automatically discarded the first time you call getSoloOutput or getMultiOutput with a lexicon
     * parameter that is not null.
     *
     * @param lexicon The lexicon to be loaded.
     * @see #addLexiconEntry
     * @see #discardLexicon
     */
    public synchronized void addLexicon(Lexicon lexicon) {
        clearMessages();
        Query q = new Query(new Compound("add_lexicon_entries", new Term[]{Util.textToTerm(lexicon.toList())}));
        q.oneSolution();
    }

    /**
     * Adds a new lexicon entry. Note that this lexicon entry is automatically discarded the first time
     * you call getSoloOutput or getMultiOutput with a lexicon parameter that is not null.
     *
     * @param lexiconEntry The lexicon entry to be added.
     * @see #addLexicon
     * @see #discardLexicon
     */
    public synchronized void addLexiconEntry(LexiconEntry lexiconEntry) {
        clearMessages();
        Query q = new Query(new Compound("add_lexicon_entry", new Term[]{Util.textToTerm(lexiconEntry.toString())}));
        q.oneSolution();
    }

    /**
     * Discards the dynamically added lexicon entries. Note that the lexicon entries that are complied into
     * the SWI Prolog executable are not affected by this operation.
     *
     * @see #addLexiconEntry
     * @see #addLexicon
     */
    public synchronized void discardLexicon() {
        clearMessages();
        Query q = new Query("discard_ulex");
        q.oneSolution();
    }

    private void clearMessages() {
        Query q = new Query("clear_messages");
        q.oneSolution();
    }


    private static String getSwiplLibraryDir()  {
        Map<String, String> swiplVars = getSwiplVariables();
        String plbase = swiplVars.get("PLBASE");
        String plarch = swiplVars.get("PLARCH");
        return plbase + "/lib/" + plarch;
    }

    private synchronized static Map<String, String> getSwiplVariables()  {
        if (swiplRuntimeVariables == null) {
            swiplRuntimeVariables = new HashMap<String, String>();
            Process process = null;
            try {
                process = Runtime.getRuntime().exec(new String[]{"swipl", "--dump-runtime-variables"});
            } catch (IOException e) {
                JPLException jpe = new JPLException("can't exec swipl --dump-runtime-variables");
                jpe.initCause(e);
                throw jpe;
            }
            try {
                InputStream in = process.getInputStream();
                BufferedReader reader = new BufferedReader(new InputStreamReader(in));
                String line;
                Pattern pattern = Pattern.compile("(^[^=]+)=\"(.+)\";$");
                while ((line = reader.readLine()) != null) {
                    Matcher matcher = pattern.matcher(line);
                    if (matcher.matches()) {
                        String var = matcher.group(1);
                        String value = matcher.group(2);
                        swiplRuntimeVariables.put(var, value);
                    }
                }
            } catch (IOException e) {
                JPLException jpe = new JPLException("error reading results of  swipl --dump-runtime-variables");
                jpe.initCause(e);
                throw jpe;
            } finally {
                try {
                    process.waitFor();
                } catch (InterruptedException ignored) {

                }
            }
            if(process.exitValue() !=0) {
                throw new JPLException("error getting swipl variables - exit code " + process.exitValue());
            }
        }
        return swiplRuntimeVariables;
    }
}