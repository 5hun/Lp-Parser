# TODO: full command line options through optparse or argparse
# clean up/double check of code
# work out elegant error handling
# focus on output objects, and document clearly

# output pulp.LpProblem instance
# need to generate the LpVariable objects, form constraints and objectives,
# and add them to an LpProblem object
# variables are not directly added to the LpProblem

# ideally we would shift any constraints from the A matrix which only pertain
# to one variable into the var bounds section

from collections import defaultdict
import sys

import pyparsing as pp
import pulp

MINIMIZE = 1
MAXIMIZE = -1

OBJ_SENSES = {
    "max": MAXIMIZE, "maximum": MAXIMIZE, "maximize": MAXIMIZE,
    "min": MINIMIZE, "minimum": MINIMIZE, "minimize": MINIMIZE
}

GEQ = 1
EQ = 0
LEQ = -1

CONSTRAINT_SENSES = {
    "<": LEQ, "<=": LEQ, "=<": LEQ,
    "=": EQ,
    ">": GEQ, ">=": GEQ, "=>": GEQ
}

INFINITY = 1E30


class Matrix:
    """ output matrix class """

    class Objective:
        def __init__(self, expression, sense, name):
            if name:
                self.name = name[0]
            else:
                self.name = ""

            # 1 is minimise, -1 is maximise
            self.sense = sense

            # a dict with variable names as keys, and coefficients as values
            self.expression = expression

    class Constraint:
        def __init__(self, expression, sense, rhs, name):
            if name:
                self.name = name[0]
            else:
                self.name = ""
            self.sense = sense  # 1 is geq, 0 is eq, -1 is leq
            self.rhs = rhs
            self.expression = expression

    class Variable:
        def __init__(self, bounds, category, name):
            self.name = name
            self.bounds = (bounds["lb"], bounds["ub"])  # a tuple (lb, ub)
            self.category = category  # 1 for int, 0 for linear

    def __init__(self,
                 parserObjective, parserConstraints,
                 parserBounds, parserGenerals, parserBinaries):

        self.objective = Matrix.Objective(
            varExprToDict(parserObjective.varExpr),
            OBJ_SENSES[parserObjective.objSense], parserObjective.name)

        self.constraints = [
            Matrix.Constraint(
                varExprToDict(c.varExpr), CONSTRAINT_SENSES[c.sense],
                c.rhs, c.name)
            for c in parserConstraints
        ]

        # can't get parser to generate this dict
        # because one var can have several bound statements
        boundDict = getBoundDict(parserBounds, parserBinaries)

        allVarNames = set()
        allVarNames.update(list(self.objective.expression.keys()))
        for c in self.constraints:
            allVarNames.update(list(c.expression.keys()))
        allVarNames.update(parserGenerals)
        allVarNames.update(list(boundDict.keys()))

        self.variables = [
            Matrix.Variable(
                boundDict[vName],
                ((vName in list(parserGenerals)) or
                 (vName in list(parserBinaries))),
                vName
            )
            for vName in allVarNames
        ]

    def __repr__(self):
        return "Objective%s\n\nConstraints (%d)%s\n\nVariables (%d)%s" % (
            "\n%s %s %s" % (
                self.objective.sense, self.objective.name,
                str(self.objective.expression)
            ),
            len(self.constraints),
            "".join([
                "\n(%s, %s, %s, %s)" % (
                    c.name, str(c.expression), c.sense, c.rhs)
                for c in self.constraints
            ]),
            len(self.variables),
            "".join([
                "\n(%s, %s, %s)" % (v.name, str(v.bounds), v.category)
                for v in self.variables
            ])
        )

    @staticmethod
    def convert_constraint(c, n2v) -> pulp.LpConstraint:
        r"""Convert Constraint object to pulp.LpConstraint"""
        lhs = pulp.lpSum([
            coef * n2v[name] for name, coef in c.expression.items()])
        if c.sense == 1:
            return pulp.LpConstraint(
                e=lhs, sense=pulp.LpConstraintGE, rhs=c.rhs,
                name=c.name if c.name else None)
        elif c.sense == 0:
            return pulp.LpConstraint(
                e=lhs, sense=pulp.LpConstraintEQ, rhs=c.rhs,
                name=c.name if c.name else None)
        else:
            assert c.sense == -1
            return pulp.LpConstraint(
                e=lhs, sense=pulp.LpConstraintLE, rhs=c.rhs,
                name=c.name if c.name else None)

    def to_pulp(self) -> pulp.LpProblem:
        r"""Convert to pulp.LpProblem"""
        if self.objective.sense == 1:
            sense = pulp.constants.LpMinimize
        else:
            sense = pulp.constants.LpMaximize
        prob = pulp.LpProblem(
            name=self.objective.name,
            sense=sense
        )

        vs = {}
        for v in self.variables:
            vs[v.name] = pulp.LpVariable(
                v.name,
                v.bounds[0] if v.bounds[0] > - INFINITY else None,
                v.bounds[1] if v.bounds[1] < INFINITY else None,
                "Continuous" if v.category == 0 else "Integer"
            )

        prob += pulp.lpSum([
            coef * vs[name]
            for name, coef in self.objective.expression.items()
        ])

        for c in self.constraints:
            prob += self.convert_constraint(c, vs)

        return prob


def varExprToDict(varExpr):
    return dict((v.name[0], v.coef) for v in varExpr)


def getBoundDict(parserBounds, parserBinaries):
    # need this versatility
    # because the lb and ub can come in separate bound statements
    boundDict = defaultdict(lambda: {"lb": -INFINITY, "ub": INFINITY})

    for b in parserBounds:
        bName = b.name[0]

        # if b.free, default is fine

        if b.leftbound:
            if CONSTRAINT_SENSES[b.leftbound.sense] >= 0:  # NUM >= var
                boundDict[bName]["ub"] = b.leftbound.numberOrInf[0]

            if CONSTRAINT_SENSES[b.leftbound.sense] <= 0:  # NUM <= var
                boundDict[bName]["lb"] = b.leftbound.numberOrInf[0]

        if b.rightbound:
            if CONSTRAINT_SENSES[b.rightbound.sense] >= 0:  # var >= NUM
                boundDict[bName]["lb"] = b.rightbound.numberOrInf[0]

            if CONSTRAINT_SENSES[b.rightbound.sense] <= 0:  # var <= NUM
                boundDict[bName]["ub"] = b.rightbound.numberOrInf[0]

    for bName in parserBinaries:
        boundDict[bName]["lb"] = 0
        boundDict[bName]["ub"] = 1

    return boundDict


def multiRemove(baseString, removables):
    """
    replaces an iterable of strings in removables
    if removables is a string, each character is removed
    """
    for r in removables:
        try:
            baseString = baseString.replace(r, "")
        except TypeError:
            raise TypeError("Removables contains a non-string element")
    return baseString


def build_grammar():
    allNameChars = pp.alphanums + "!\"#$%&()/,.;?@_'`{}|~"

    # can probably use CharsNotIn instead
    firstChar = multiRemove(allNameChars, pp.nums + "eE.")

    name = pp.Word(firstChar, allNameChars, max=255)
    keywords = [
        "inf", "infinity", "max", "maximum", "maximize", "min", "minimum",
        "minimize", "s.t.", "st", "bound", "bounds", "bin", "binaries",
        "binary", "gen",  "general", "end"
    ]
    pyKeyword = pp.MatchFirst(list(map(pp.CaselessKeyword, keywords)))
    validName = ~pyKeyword + name
    validName = validName.setResultsName("name")

    colon = pp.Suppress(pp.oneOf(": ::"))
    plusMinus = pp.oneOf("+ -")
    inf = pp.oneOf("inf infinity", caseless=True)
    number = pp.Word(pp.nums + ".")
    sense = pp.oneOf("< <= =< = > >= =>").setResultsName("sense")

    # section tags
    objTagMax = pp.oneOf("max maximum maximize", caseless=True)
    objTagMin = pp.oneOf("min minimum minimize", caseless=True)
    objTag = (objTagMax | objTagMin).setResultsName("objSense")

    constraintsTag = pp.oneOf(
        ["subj to", "subject to", "s.t.", "st"], caseless=True)

    boundsTag = pp.oneOf("bound bounds", caseless=True)
    binTag = pp.oneOf("bin binaries binary", caseless=True)
    genTag = pp.oneOf("gen general", caseless=True)

    endTag = pp.CaselessLiteral("end")

    # coefficient on a variable (includes sign)
    firstVarCoef = pp.Optional(plusMinus, "+") + pp.Optional(number, "1")
    # TODO: can't this just be eval(tokens[0] + tokens[1])?
    firstVarCoef.setParseAction(lambda tokens: eval("".join(tokens)))

    coef = plusMinus + pp.Optional(number, "1")
    # TODO: can't this just be eval(tokens[0] + tokens[1])?
    coef.setParseAction(lambda tokens: eval("".join(tokens)))

    # variable (coefficient and name)
    firstVar = pp.Group(firstVarCoef.setResultsName("coef") + validName)
    var = pp.Group(coef.setResultsName("coef") + validName)

    # expression
    varExpr = firstVar + pp.ZeroOrMore(var)
    varExpr = varExpr.setResultsName("varExpr")

    # objective
    objective = pp.Group(objTag + pp.Optional(validName + colon) + varExpr)
    objective = objective.setResultsName("objective")

    # constraint rhs
    rhs = pp.Optional(plusMinus, "+") + number
    rhs = rhs.setResultsName("rhs")
    rhs.setParseAction(lambda tokens: eval("".join(tokens)))

    # constraints
    constraint = pp.Group(
        pp.Optional(validName + colon) + varExpr + sense + rhs)
    constraints = pp.ZeroOrMore(constraint)
    constraints = constraints.setResultsName("constraints")

    # bounds
    signedInf = (plusMinus + inf).setParseAction(
        lambda tokens: -1 ** (tokens[0] == "-") * INFINITY)
    # this is different to previous,
    # because "number" is mandatory not pp.Optional
    signedNumber = (pp.Optional(plusMinus, "+") + number).setParseAction(
        lambda tokens: eval("".join(tokens)))
    numberOrInf = (signedNumber | signedInf).setResultsName("numberOrInf")
    sensestmt = pp.Group(
        pp.Optional(
            pp.Group(numberOrInf & sense).setResultsName("leftbound")) +
        validName +
        pp.Optional(pp.Group(numberOrInf & sense).setResultsName("rightbound"))
    )
    freeVar = pp.Group(validName + pp.Literal("free"))

    boundstmt = freeVar | sensestmt
    bounds = boundsTag + pp.ZeroOrMore(boundstmt).setResultsName("bounds")

    # generals
    generals = genTag + pp.ZeroOrMore(validName).setResultsName("generals")

    # binaries
    binaries = binTag + pp.ZeroOrMore(validName).setResultsName("binaries")

    varInfo = pp.ZeroOrMore(bounds | generals | binaries)

    grammar = objective + constraintsTag + constraints + varInfo + endTag

    # commenting
    commentStyle = pp.Literal("\\") + pp.restOfLine
    grammar.ignore(commentStyle)
    return grammar


def read(filename, pulp=True):
    # read input lp file
    try:
        fp = open(filename)
        fullDataString = fp.read()
        fp.close()
    except IOError:
        print("Could not find input lp file \"%s\"" % filename)
        raise IOError
    grammar = build_grammar()
    # parse input string
    parseOutput = grammar.parseString(fullDataString)

    # create generic output Matrix object
    m = Matrix(
        parseOutput.objective, parseOutput.constraints, parseOutput.bounds,
        parseOutput.generals, parseOutput.binaries)

    if pulp:
        return m.to_pulp()
    else:
        return m


if __name__ == "__main__":
    try:
        sys.argv[1]
    except IndexError:
        print("Usage: $ python lp_parser.py <lpfile>")
        sys.exit()

    m = read(sys.argv[1])
    print(m)
