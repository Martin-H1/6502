import string
import sys

class Token:
    UNKNOWN = 0
    COMMENT = 1
    COMMENT_LINE = 2
    EOL = 3
    IDENTIFIER = 4
    KEYWORD = 5
    LABEL = 6
    LITERAL = 7
    OPERATOR = 8
    SEPARATOR = 9
    STRING = 10

    NAMES = ["UNKNOWN", "COMMENT", "COMMENT_LINE", "EOL", "IDENTIFIER", "KEYWORD", "LABEL", "LITERAL", "OPERATOR", "SEPARATOR", "STRING"]

    TYPE_FORMAT = {
        UNKNOWN : '{}',
        COMMENT : '\t\t;{}',
        COMMENT_LINE : ';{}',
        IDENTIFIER : '{}',
        KEYWORD : '\t{} ',
        LABEL : '{}:',
        LITERAL : '{}',
        OPERATOR : '{}',
        SEPARATOR : '{}',
        STRING : '"{}"',
        EOL : '{}'
    }

    def __init__(self, type = UNKNOWN, value = ''):
        self.type = type
        self.value = value

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.type == other.type and self.value == other.value
        else:
            return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __str__(self):
        return "type='{}', value='{}'".format(Token.NAMES[self.type], self.value)
    def appendChar(self, first):
        self.value = self.value + first

    def prettyPrint(self):
        print(Token.TYPE_FORMAT[self.type].format(self.value), end='')

class Tokenizer:
    def __init__(self, comment, keywords, operators, separators):
        """
        Initializer that sets up the tokenizer for processing. It also
        accepts varaious parameters that demarcate tokens. Note this class
        was borrowed from another project, so it does more than needed

        Arguments:
        comment -- the character(s) that indicate a comment line.
        keywords -- a list of languages keywords for classifications.
        operators -- the characters that are operators for classification.
        separators -- the characters that separate elements of syntax.
        """

        self.comment = comment
        self.keywords = keywords
        self.operators = operators
        self.separators = separators
        self.tokenList = []
        self.tokenDispatch = {
            Token.UNKNOWN : self.processUnknown,
            Token.COMMENT : self.processComment,
            Token.COMMENT_LINE : self.processComment,
            Token.IDENTIFIER : self.processIdentifier,
            Token.LABEL : self.processLabel,
            Token.LITERAL : self.processLiteral,
            Token.STRING : self.processString
        }
        self.workToken = Token()

    def tokenizeFile(self, filename):
        """
        Opens a file by name and tokenizees it a line at a time.

        Arguments:
        filename -- the name of the file.
        """
        self.workToken = Token()
        with open(filename) as sourcefile:
            for line in sourcefile:
                self.tokenizeLine(line)
                self.appendToken(Token.EOL, "\n")

    def tokenizeLine(self, line):
        start = True
        while line != "":
            dispatch = self.tokenDispatch.get(self.workToken.type)
            line = dispatch(start, line[:1], line[1:], line)
            start = False

    """
    The following methods are dispatchers that all have the same
    argument signature. This allows calling them via a dispatch
    table based upon the type of token identified.
    """
    def processUnknown(self, start, first, rest, line):
        """
        Default state for the tokenizer. If it identifies the token type
        it will transition to that state which results in that dispatcher
        getting called.

        Arguments:
        first -- the first character in the line.
        rest -- the rest of the chacters in the line.
        line -- the entire line to parse.

        Returns:
        the characters that were not consumed by the processor.
        """
        if start and line.startswith(self.comment):
            self.workToken.type = Token.COMMENT_LINE
            rest = line[len(self.comment):]
        elif line.startswith(self.comment):
            self.workToken.type = Token.COMMENT
            rest = line[len(self.comment):]
        elif start and (first.isalpha() or first == "_"):
            self.workToken.type = Token.LABEL
            self.workToken.appendChar(first)
        elif first.isalpha() or first == "_" or first == ".":
            self.workToken.type = Token.IDENTIFIER
            self.workToken.appendChar(first)
        elif first.isnumeric() or first == "$" or first == "%":
            self.workToken.type = Token.LITERAL
            self.workToken.appendChar(first)
        elif first in self.separators:
            self.appendToken(Token.SEPARATOR, first)
        elif first in self.operators:
            # check for C style two character operators (e.g. ++, --, ==, etc)
            if first + rest[:1] in self.operators:
                first = first + rest[:1]
                rest = rest[1:]
            # Convert * to ^ as required by Ophis.
            if first == "*":
                first = "^"

            self.appendToken(Token.OPERATOR, first)
        elif first == "'" or first == '"':
            self.workToken.type = Token.STRING
        return rest

    def processComment(self, start, first, rest, line):
        if first == "\n":
            self.appendToken(self.workToken.type, self.workToken.value)
        else:
            self.workToken.appendChar(first)
        return rest

    def processLabel(self, start, first, rest, line):
        if first.isalnum() or first == "_":
            self.workToken.appendChar(first)
            return rest
        elif not first.isalpha():
            self.appendToken(self.workToken.type, self.workToken.value)
            return line

    def processIdentifier(self, start, first, rest, line):
        if first.isalnum() or first == "_":
            self.workToken.appendChar(first)
            return rest
        elif not first.isalpha():
            if self.workToken.value in self.keywords:
                self.appendToken(Token.KEYWORD, self.workToken.value)
            else:
                self.appendToken(self.workToken.type, self.workToken.value)
            return line

    def processLiteral(self, start, first, rest, line):
        if first.isnumeric() or first in "ABCDEFabcdef":
            self.workToken.appendChar(first)
            return rest
        else:
            self.appendToken(self.workToken.type, self.workToken.value)
            return line

    def processString(self, start, first, rest, line):
        if first != "'" and first != "\"":
            self.workToken.appendChar(first)
        elif first == "'" or first == "\"":
            self.appendToken(Token.STRING, self.workToken.value)
        return rest

    def appendToken(self, type, value):
        """
        Adds a token to the end of the list and resets the working token.

        Arguments:
        type -- the type of the token to add.
        value -- the text contents of the token.
        """

        self.workToken.type = type
        self.workToken.value = value
        self.tokenList.append(self.workToken)

        # Reset tokenizer state.
        self.workToken = Token()

opcodes = [".alias", ".BYTE", ".WORD", ".org", "ADC", "AND", "ASL", "BCC", "BCS", "BEQ", "BIT", "BMI", "BNE", "BPL", "BRK", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV", "CMP", "CPX", "CPY", "DEC", "DEX", "DEY", "EOR", "INC", "INX", "INY", "JMP", "JSR", "LDA", "LDX", "LDY", "LSR", "NOP", "ORA", "PHA", "PHP", "PLA", "PLP", "ROL", "ROR", "RTI", "RTS", "SBC", "SEC", "SED", "SEI", "STA", "STX", "STY", "STZ", "TAX", "TAY", "TSX", "TXA", "TXS", "TYA", ".byte", ".word", ".org", "adc", "and", "asl", "bcc", "bcs", "beq", "bit", "bmi", "bne", "bpl", "brk", "bvc", "bvs", "clc", "cld", "cli", "clv", "cmp", "cpx", "cpy", "dec", "dex", "dey", "eor", "inc", "inx", "iny", "jmp", "jsr", "lda", "ldx", "ldy", "lsr", "nop", "ora", "pha", "php", "pla", "plp", "rol", "ror", "rti", "rts", "sbc", "sec", "sed", "sei", "sta", "stx", "sty", "stz", "tax", "tay", "tsx", "txa", "txs", "tya"]

operators = ["+", "-", "*", "<", ">", "#", "(", ")", ",", "="]

try:
    tokenizer = Tokenizer(";", opcodes, operators, "")
    tokenizer.tokenizeFile(sys.argv[1])
    for token in tokenizer.tokenList:
        token.prettyPrint()

except Exception as ex:
    print(ex)
