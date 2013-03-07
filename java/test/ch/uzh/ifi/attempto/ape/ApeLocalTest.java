package ch.uzh.ifi.attempto.ape;

import jpl.Compound;
import jpl.JPLException;
import jpl.Term;
import jpl.Variable;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Created with IntelliJ IDEA.
 * User: ses
 * Date: 2/2/13
 * Time: 2:54 AM
 * To change this template use File | Settings | File Templates.
 */

public class ApeLocalTest extends Testcase {
    private static Log logger = LogFactory.getLog(ApeLocalTest.class);

    @Test
    public void testCreateLocal() throws ACEParserException {
        APELocal apeLocal = getApeLocal();
        assertNotNull(apeLocal);
    }

    private APELocal getApeLocal() {
        APELocal.init("../ape.exe");
        return APELocal.getInstance();
    }

    @Test
    public void testDRS() throws ACEParserException {
        APELocal apeLocal = getApeLocal();
        apeLocal.setGuessingEnabled(true);
        String drs = apeLocal.getSoloOutput(ACETEXT, OutputType.DRS);
        assertEquals("DRS mismatch", ACETEXT_DRS, drs);
        drs = apeLocal.getSoloOutput("Every dog is an animal. Lucas is a dog.", OutputType.DRS);
        Compound term = (Compound) jpl.Util.textToTerm(drs);
        DRSItem bar = convertTermToDRSItem(term);
        logger.info(bar);

    }

    static DRSItemList convertListToDRSItems(Term cons) {
        DRSItemList result = new DRSItemList();
        return convertListToDRSItems(cons, result);
    }

    static DRSItemList convertListToDRSItems(Term cons, DRSItemList result) {
        if (cons.name().equals("[]")) {
            return result;
        } else if (cons.isCompound() && cons.name().equals(".")) {
            Compound compound = (Compound) cons;
            if (compound.arity() != 2) {
                throw new JPLException("wrong arity");
            }
            result.add(convertTermToDRSItem(compound.arg(1)));
            return convertListToDRSItems(compound.arg(2), result);
        } else {
            throw new JPLException("Not a proper list");
        }
    }

    static DRSItem convertTermToDRSItem(Term term) {
        if (term instanceof Variable) {
            Variable variable = (Variable) term;
            return new DRSVar(variable.name());
        }
        if (term.name().equals("drs")) {
            DRS result = new DRS();
            result.vars = (DRSItemList)convertTermToDRSItem(term.arg(1));
            result.body = (DRSItemList)convertTermToDRSItem(term.arg(2));
            return result;
        } else if (term.name().equals(".")) {
               return convertListToDRSItems(term);
        } else if (term.name().equals("=>")) {
            DRS antecedent = (DRS)convertTermToDRSItem(term.arg(1));
            DRS consequent = (DRS)convertTermToDRSItem(term.arg(2));
            return new DRSImplication(antecedent,consequent);
        } else if (term.name().equals("-")) {
            Term atom = term.arg(1);
            Term location = term.arg(2);
            if(!location.name().equals("/")) {
                throw new JPLException("expected /, got " + location.toString());
            }
            int sid = location.arg(1).intValue();
            int tid = location.arg(2).intValue();
            return new DRSAtom(atom,sid,tid);
        }

        else {
            throw new JPLException("bad translate of DRS term: " + term.toString());
        }
    }
    static class DRSItem {}
    static class DRSAtom extends DRSItem {
        Term term;
        int sentenceId;
        int tokenId;

        DRSAtom(Term term, int sentenceId, int tokenId) {
            this.term = term;
            this.sentenceId = sentenceId;
            this.tokenId = tokenId;
        }
        public String toString() {
            return "'" + term.toString() + "'";
        }
    }
    static class DRS extends DRSItem{
        DRSItemList vars;
        DRSItemList body;

        @Override
        public String toString() {
            return "DRS{" +
                    "vars=" + vars +
                    ", body=" + body +
                    '}';
        }
    }

    static class DRSVar extends DRSItem {
        String name;
        DRSVar(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return "?" + name;
        }
    }
    static class DRSImplication extends DRSItem {
        DRS antecedent;
        DRS consequent;

        DRSImplication(DRS antecedent, DRS consequent) {
            this.antecedent = antecedent;
            this.consequent = consequent;
        }

        @Override
        public String toString() {
            return "DRSImplication{" +
                    "antecedent=" + antecedent +
                    ", consequent=" + consequent +
                    '}';
        }
    }
    static class DRSItemList extends DRSItem implements List<DRSItem> {
         ArrayList<DRSItem> delegate = new ArrayList<DRSItem>();

        public void ensureCapacity(int i) {
            delegate.ensureCapacity(i);
        }

        @Override
        public int size() {
            return delegate.size();
        }

        @Override
        public boolean isEmpty() {
            return delegate.isEmpty();
        }

        @Override
        public boolean contains(Object o) {
            return delegate.contains(o);
        }

        @Override
        public int indexOf(Object o) {
            return delegate.indexOf(o);
        }

        @Override
        public int lastIndexOf(Object o) {
            return delegate.lastIndexOf(o);
        }

        @Override
        public Object clone() {
            return delegate.clone();
        }

        @Override
        public Object[] toArray() {
            return delegate.toArray();
        }

        @Override
        public <T> T[] toArray(T[] ts) {
            return delegate.toArray(ts);
        }

        @Override
        public DRSItem get(int i) {
            return delegate.get(i);
        }

        @Override
        public DRSItem set(int i, DRSItem drsItem) {
            return delegate.set(i, drsItem);
        }

        @Override
        public boolean add(DRSItem drsItem) {
            return delegate.add(drsItem);
        }

        @Override
        public void add(int i, DRSItem drsItem) {
            delegate.add(i, drsItem);
        }

        @Override
        public DRSItem remove(int i) {
            return delegate.remove(i);
        }

        @Override
        public boolean remove(Object o) {
            return delegate.remove(o);
        }

        @Override
        public void clear() {
            delegate.clear();
        }

        @Override
        public boolean addAll(Collection<? extends DRSItem> drsItems) {
            return delegate.addAll(drsItems);
        }

        @Override
        public boolean addAll(int i, Collection<? extends DRSItem> drsItems) {
            return delegate.addAll(i, drsItems);
        }

        @Override
        public Iterator<DRSItem> iterator() {
            return delegate.iterator();
        }

        @Override
        public ListIterator<DRSItem> listIterator() {
            return delegate.listIterator();
        }

        @Override
        public ListIterator<DRSItem> listIterator(int i) {
            return delegate.listIterator(i);
        }

        @Override
        public List<DRSItem> subList(int i, int i2) {
            return delegate.subList(i, i2);
        }

        @Override
        public boolean equals(Object o) {
            return delegate.equals(o);
        }

        @Override
        public int hashCode() {
            return delegate.hashCode();
        }

        @Override
        public boolean containsAll(Collection<?> objects) {
            return delegate.containsAll(objects);
        }

        @Override
        public boolean removeAll(Collection<?> objects) {
            return delegate.removeAll(objects);
        }

        @Override
        public boolean retainAll(Collection<?> objects) {
            return delegate.retainAll(objects);
        }

        @Override
        public String toString() {
            return delegate.toString();
        }

        public void trimToSize() {
            delegate.trimToSize();
        }
    }

}
