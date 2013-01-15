package ch.uzh.ifi.attempto.ape;

public class Testcase {

	public static final String ACETEXT = "John waits.";
	public static final String ACETEXT_DRSPP = "[A]\npredicate(A,wait,named(John))-1/2";
	public static final String ACETEXT_DRS = "drs([A],[predicate(A,wait,named('John'))-1/2])";
	public static final String ACETEXT_TPTP = "fof(f1, axiom, (\n? [A] : (predicate1(A,wait,'John')))).";

	public static final String ACETEXT1 = "Every dog's friend is an animal.";
	public static final String ACETEXT1_CORE_ACE = "If there is a friend X1 of a dog then the friend X1 is an animal.";

	public static final String ACETEXT2 = "Every dooog's friend is an animal.";
	public static final String ACETEXT2_CORE_ACE = "If there is a friend X1 of a n:dooog then the friend X1 is an animal.";

	public static final String ACETEXT3 = "John|pn_sg|John_PN| |tv_finsg|buy_V2 everything that | isn't a present|noun_sg|present_N .";
	public static final Object ACETEXT3_OWLFSS = "'Ontology'(test,['SubClassOf'('ObjectComplementOf'('':present_N),'ObjectSomeValuesFrom'('ObjectInverseOf'('':buy_V2),'ObjectOneOf'(['':'John_PN'])))])";
}